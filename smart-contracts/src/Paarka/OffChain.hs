{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Paarka.OffChain (
    SaleParams(..), BuyParams(..),
    StartSaleSchema, BuySchema,
    startSale, buy,
    startSaleEndpoint, buyEndpoints
) where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Map               as Map
import           Data.Monoid            (Last (..))
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import           Ledger.Value           as Value
import           Plutus.Contract        as Contract
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Schema                 (ToSchema)

import           Text.Printf            (printf)
import           Prelude                (Semigroup (..), Show (..), String, (<>))
import qualified Prelude

import           Paarka.Utils           (SaleParams(..), BuyParams(..))
import           Paarka.PaarkaCoin      (paarkaSymbol, paarkaPolicy)
import           Paarka.AccessToken     (nftTokenSymbol, nftTokenPolicy)
import           Paarka.OnChain         as OnChain
import           Paarka.Types           (Paarka, PaarkaRedeemer(..))

-- | Contracts

startSale :: forall w s. SaleParams -> Contract w s Text SaleParams
startSale sp = do
    pkh   <- pubKeyHash <$> Contract.ownPubKey
    let v = Value.singleton (currency sp) (token sp) 1
        tx = mustPayToTheScript () v
    ledgerTx <- submitTxConstraints (OnChain.typedValidator sp) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "sale started for token %s" (show sp)
    return sp

findPkh :: Contract (Last PubKeyHash) s Text ()
findPkh = do
    pkh   <- pubKeyHash <$> Contract.ownPubKey
    logInfo @String $ printf "pubkeyhash is %s" (show pkh)
    tell $ Last $ Just pkh

findSale :: SaleParams -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut))
findSale sp = do
    utxos <- utxosAt $ OnChain.paarkaAddress sp
    return $ do
        (oref, o) <- find f $ Map.toList utxos
        return (oref, o)
  where
    f :: (TxOutRef, ChainIndexTxOut) -> Bool
    f (_, o) = assetClassValueOf (txOutValue $ toTxOut o) (assetClass (currency sp) (token sp)) == 1

buy :: SaleParams -> Integer -> PubKeyHash -> Contract w s Text ()
buy sp amount buyer = do
    m <- findSale sp
    case m of
        Nothing      -> Contract.logInfo @String $ printf "sale not found"
        Just(oref, o)-> do
            let tn = "PaarkaCoin"
                paarka  = Value.singleton paarkaSymbol tn amount
                owner : _ = (ownerPkh sp)
                val     = Value.singleton (nftTokenSymbol sp) (token sp) 1
                lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                          Constraints.otherScript (OnChain.validator sp) <>
                          Constraints.mintingPolicy paarkaPolicy <>
                          Constraints.mintingPolicy (nftTokenPolicy sp)
                tx      = Constraints.mustMintValue paarka <>
                          Constraints.mustPayToPubKey owner paarka <>
                          Constraints.mustMintValue val <>
                          Constraints.mustPayToPubKey buyer val <>
                          Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Buy buyer) <>
                          Constraints.mustPayToOtherScript (OnChain.valHash sp) (Datum $ PlutusTx.toBuiltinData ()) (txOutValue $ toTxOut o)
            ledgerTx <- submitTxConstraintsWith @Paarka lookups tx
            awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "minted %s tokens" (show paarka)
            logInfo @String $ printf "purchase done %s with amount" (show sp)

-- | Endpoints

type StartSaleSchema = Endpoint "start" SaleParams

startSaleEndpoint :: Contract (Last SaleParams) StartSaleSchema Text ()
startSaleEndpoint = forever
              $ handleError logError
              $ awaitPromise start' >>= tell . Last . Just
    where
        start' = endpoint @"start" $ \sp -> startSale sp

type BuySchema =
        Endpoint "buy" BuyParams
    .\/ Endpoint "find-pkh" ()

buyEndpoints :: Contract (Last PubKeyHash) BuySchema Text ()
buyEndpoints = forever
            $ handleError logError
            $ awaitPromise $ buy' `select` findPkh'
    where
        buy' = endpoint @"buy" $ \bp -> buy (nftSale bp) (amt bp) (buyerPkh bp)
        findPkh' = endpoint @"find-pkh" $ const findPkh
