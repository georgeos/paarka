{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Main
    ( main
    ) where

import           Control.Monad                       (forM_, void, when)
import           Control.Monad.Freer                 (Eff, Member, interpret, type (~>))
import           Control.Monad.Freer.Error           (Error)
import           Control.Monad.Freer.Extras.Log      (LogMsg)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON, Result (..), fromJSON)
import           Data.Default                        (Default (..))
import           Data.Monoid                         (Last (..))
import           Data.Text                           (Text, pack)
import           Ledger
import           Ledger.Constraints
import qualified Ledger.Value                        as Value
import           Plutus.Contract
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), endpointsToSchemas, handleBuiltin)
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import qualified Plutus.Contracts.Currency           as Currency

import           Wallet.Emulator.Types               (Wallet (..), walletPubKey)
import           Wallet.Types                        (ContractInstanceId (..))

import qualified Paarka.Paarka                       as Paarka
import           Paarka.PAB                          (PaarkaContracts (..))

main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin PaarkaContracts) "Starting Paarka PAB webserver. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    cidInit <- Simulator.activateContract (Wallet 1) Init
    cs      <- waitForLast cidInit
    _       <- Simulator.waitUntilFinished cidInit

    cidPaarka <- Simulator.activateContract (Wallet 1) $ Sale cs
    liftIO $ writeFile "paarka.cid" $ show $ unContractInstanceId cidPaarka
    paarka <- waitForLast cidPaarka

    void $ liftIO getLine
    shutdown

waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid =
    flip Simulator.waitForState cid $ \json -> case fromJSON json of
        Success (Last (Just x)) -> Just x
        _                       -> Nothing

wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 5]]

usdt :: TokenName
usdt = "USDT"

saleParams :: CurrencySymbol -> Sale.SaleParams
saleParams cs = Sale.SaleParams
    { Sale.opFees   = 1_000_000
    , Sale.opSymbol = cs
    , Sale.opToken  = usdt
    }

handlePaarkaContracts ::
    ( Member (Error PABError) effs
    , Member (LogMsg (PABMultiAgentMsg (Builtin PaarkaContracts))) effs
    )
    => ContractEffect (Builtin PaarkaContracts)
    ~> Eff effs
handlePaarkaContracts = handleBuiltin getSchema getContract where
    getSchema = \case
        Init    -> endpointsToSchemas @Empty
        Sale _  -> endpointsToSchemas @Sale.SaleSchema
    getContract = \case
        Init    -> SomeBuiltin   initContract
        Sale cs -> SomeBuiltin $ Paarka.runSale $ saleParams cs

handlers :: SimulatorEffectHandlers (Builtin PaarkaContracts)
handlers =
    Simulator.mkSimulatorHandlers @(Builtin PaarkaContracts) def []
    $ interpret handlePaarkaContracts

initContract :: Contract (Last CurrencySymbol) Empty Text ()
initContract = do
    ownPK <- pubKeyHash <$> ownPubKey
    cur   <-
        mapError (pack . show)
        (Currency.mintContract ownPK [(usdt, fromIntegral (length wallets) * amount)]
        :: Contract (Last CurrencySymbol) Empty Currency.CurrencyError Currency.OneShotCurrency)
    let cs = Currency.currencySymbol cur
        v  = Value.singleton cs usdt amount
    forM_ wallets $ \w -> do
        let pkh = pubKeyHash $ walletPubKey w
        when (pkh /= ownPK) $ do
            tx <- submitTx $ mustPayToPubKey pkh v
            awaitTxConfirmed $ txId tx
    tell $ Last $ Just cs
  where
    amount :: Integer
    amount = 100_000_000
