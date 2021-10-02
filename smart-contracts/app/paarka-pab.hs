{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
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

import           Control.Monad                       (void)
import           Control.Monad.Freer                 (interpret)
import           Control.Monad.IO.Class              (MonadIO (..))

import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.Default                        (Default (..))
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)

import           GHC.Generics                        (Generic)

import           Language.PureScript.Bridge          (equal, genericShow, mkSumType)

import           Playground.Types                    (FunctionSchema)

import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Plutus.PAB.Run.PSGenerator          (HasPSTypes (..))
import           Schema                              (FormSchema)
import qualified Paarka.Paarka                       as Paarka
import qualified Paarka.NFT                          as NFT



-- | PAB has changed from lesson 06
-- There is an example in plutus repo: plutus/plutus-pab/examples/ContractExample.hs
-- In someway we have to (similar to oracle-pab):
--  - Mint one NFT
--  - Create the Sale using one Wallet
-- Regarding to lesson 6 (2:09:00), paarka-client.hs shouldn't necessary because this paarka-pab should start PAB Web Server
-- Regarding to lesson 6 (2:04:00), we dont need to activateContracts because we can do it from the "web server" using HTTP requests
main :: IO ()
main = void $ Simulator.runSimulationWith handlers $ do
    Simulator.logString @(Builtin PaarkaContracts) "Starting plutus-starter PAB webserver on port 8080. Press enter to exit."
    shutdown <- PAB.Server.startServerDebug

    void $ liftIO getLine

    Simulator.logString @(Builtin PaarkaContracts) "Balances at the end of the simulation"
    b <- Simulator.currentBalances
    Simulator.logBalances @(Builtin PaarkaContracts) b

    shutdown

data PaarkaContracts = MintNFT
                     | StartSale
                     | Buy
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance Pretty PaarkaContracts where
    pretty = viaShow

instance HasPSTypes PaarkaContracts where
    psTypes p =
        [ (equal <*> (genericShow <*> mkSumType)) p
        -- These types come from the Paarka contract and need to be available in PS
        -- , (equal <*> (genericShow <*> mkSumType)) (Proxy @Paarka.SaleParams)
        -- , (equal <*> (genericShow <*> mkSumType)) (Proxy @Sale)
        ]


-- | StartSale and Buy must have parameters because getPaarkaContractsSchema and getPaarkaContracts
-- I think there should be an additional Definition like: Init (similar to oracle-pab) in order to initialize wallets
instance HasDefinitions PaarkaContracts where
    getDefinitions = [MintNFT, StartSale, Buy]
    getContract = getPaarkaContracts
    getSchema = getPaarkaContractsSchema

-- | StartSale and Buy must have parameters
getPaarkaContractsSchema :: PaarkaContracts -> [FunctionSchema FormSchema]
getPaarkaContractsSchema = \case
    MintNFT   -> Builtin.endpointsToSchemas @NFT.NFTSchema
    StartSale -> Builtin.endpointsToSchemas @Paarka.StartSaleSchema
    Buy       -> Builtin.endpointsToSchemas @Paarka.BuySchema

-- | StartSale and Buy must have parameters to pass into Paarka.startSale and Paarka.buy contracts
getPaarkaContracts :: PaarkaContracts -> SomeBuiltin
getPaarkaContracts = \case
    MintNFT   -> SomeBuiltin $ NFT.nftEndpoint
    StartSale -> SomeBuiltin $ Paarka.startSaleEndpoint
    Buy       -> SomeBuiltin $ Paarka.buyEndpoints

handlers :: SimulatorEffectHandlers (Builtin PaarkaContracts)
handlers =
    Simulator.mkSimulatorHandlers def def (interpret (contractHandler Builtin.handleBuiltin))