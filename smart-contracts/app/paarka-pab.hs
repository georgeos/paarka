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
import           Playground.Types                    (FunctionSchema)
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Schema                              (FormSchema)

import qualified Paarka.OffChain                     as Paarka
import qualified NFT.OffChain                        as NFT

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

instance HasDefinitions PaarkaContracts where
    getDefinitions = [MintNFT, StartSale, Buy]
    getContract = getPaarkaContracts
    getSchema = getPaarkaContractsSchema

getPaarkaContractsSchema :: PaarkaContracts -> [FunctionSchema FormSchema]
getPaarkaContractsSchema = \case
    MintNFT   -> Builtin.endpointsToSchemas @NFT.NFTSchema
    StartSale -> Builtin.endpointsToSchemas @Paarka.StartSaleSchema
    Buy       -> Builtin.endpointsToSchemas @Paarka.BuySchema

getPaarkaContracts :: PaarkaContracts -> SomeBuiltin
getPaarkaContracts = \case
    MintNFT   -> SomeBuiltin $ NFT.nftEndpoint
    -- | In some way this endpoint should return the Sale in the HTTP request
    -- Using this returned Sale, we can use the buyEndpoints easily
    -- Check notes in startSaleEndpoint
    StartSale -> SomeBuiltin $ Paarka.startSaleEndpoint
    Buy       -> SomeBuiltin $ Paarka.buyEndpoints

handlers :: SimulatorEffectHandlers (Builtin PaarkaContracts)
handlers =
    Simulator.mkSimulatorHandlers def def (interpret (contractHandler Builtin.handleBuiltin))