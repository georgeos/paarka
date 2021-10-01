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
import           Data.Data                           (Proxy (Proxy))
import           Data.Default                        (Default (..))
import           Data.Monoid                         (Last (..))
import           Data.Text                           (Text, pack)
import           Language.PureScript.Bridge          (equal, genericShow, mkSumType)
import           Ledger
import           Ledger.Constraints
import qualified Ledger.Value                        as Value
import           Playground.Types                    (FunctionSchema)
import           Plutus.Contract
import           Plutus.PAB.Effects.Contract         (ContractEffect (..))
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import           Plutus.PAB.Types                    (PABError (..))
import           Plutus.PAB.Run                      (runWith)
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import qualified Plutus.Contracts.Currency           as Currency
import           Plutus.PAB.Run.PSGenerator          (HasPSTypes (..))
import           Wallet.Emulator.Types               (Wallet (..), walletPubKey)
import           Wallet.Types                        (ContractInstanceId (..))
import qualified Paarka.Paarka                       as Paarka
import           Paarka.Utils                        (paarkaPkh, Sale(..))
import           Paarka.PAB                          (PaarkaContracts (..))
import           Schema                              (FormSchema)

-- | PAB has changed from lesson 06
-- There is an example in plutus repo: plutus/plutus-pab/examples/ContractExample.hs
-- In someway we have to (similar to oracle-pab):
--  - Mint one NFT
--  - Create the Sale using one Wallet
-- Regarding to lesson 6 (2:09:00), paarka-client.hs shouldn't necessary because this paarka-pab should start PAB Web Server
-- Regarding to lesson 6 (2:04:00), we dont need to activateContracts because we can do it from the "web server" using HTTP requests
main :: IO ()
main = do
    runWith (Builtin.handleBuiltin @PaarkaContracts)

instance HasPSTypes PaarkaContracts where
    psTypes p =
        [ (equal <*> (genericShow <*> mkSumType)) p
        -- These types come from the Uniswap contract and need to be available in PS
        -- , (equal <*> (genericShow <*> mkSumType)) (Proxy @Paarka.SaleParams)
        -- , (equal <*> (genericShow <*> mkSumType)) (Proxy @Sale)
        ]

-- | StartSale and Buy must have parameters because getPaarkaContractsSchema and getPaarkaContracts
-- I think there should be an additional Definition like: Init (similar to oracle-pab) in order to initialize wallets
instance HasDefinitions PaarkaContracts where
    getDefinitions = [ StartSale
                     , Buy
                     ]
    getContract = getPaarkaContracts
    getSchema = getPaarkaContractsSchema

-- | StartSale and Buy must have parameters
getPaarkaContractsSchema :: PaarkaContracts -> [FunctionSchema FormSchema]
getPaarkaContractsSchema = \case
    StartSale -> Builtin.endpointsToSchemas @Paarka.StartSaleSchema
    Buy       -> Builtin.endpointsToSchemas @Paarka.SaleSchema

-- | StartSale and Buy must have parameters to pass into Paarka.startSale and Paarka.buy contracts
getPaarkaContracts :: PaarkaContracts -> SomeBuiltin
getPaarkaContracts = \case
    StartSale saleparams        -> SomeBuiltin Paarka.startSale $ Paarka.SaleParams saleparams
    Buy       sale amount buyer -> SomeBuiltin $ Paarka.buy $ Sale sale $ Integer amount $ Ledger.PubKeyHash buyer

handlers :: SimulatorEffectHandlers (Builtin PaarkaContracts)
handlers =
    Simulator.mkSimulatorHandlers def def (interpret (contractHandler Builtin.handleBuiltin))