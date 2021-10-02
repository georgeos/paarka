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

import           Paarka.Utils           (Sale(..))
import           Paarka.PaarkaCoin      (paarkaSymbol, paarkaPolicy)
import           Paarka.AccessToken     (nftTokenSymbol, nftTokenPolicy)
import           Paarka.OnChain         as OnChain
import           Paarka.Types           (Paarka, PaarkaRedeemer(..))


data SaleParams = SaleParams {
     sCurrency :: !CurrencySymbol
    ,sToken    :: !TokenName
} deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

data BuyParams = BuyParams {
     nftSale  :: !Sale
    ,amt      :: !Integer
    ,buyerPkh :: !PubKeyHash
} deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)


-- | Contracts

startSale :: forall w s. SaleParams -> Contract w s Text Sale
startSale sp = do
    pkh   <- pubKeyHash <$> Contract.ownPubKey
    let v = Value.singleton (sCurrency sp) (sToken sp) 1
        tx = mustPayToTheScript () v
        sale = Sale {
                ownerPkh = pkh,
                currency = sCurrency sp,
                token = sToken sp
            }
    ledgerTx <- submitTxConstraints (OnChain.typedValidator sale) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "sale started for token %s" (show sale)
    return sale

findPkh :: Contract (Last PubKeyHash) s Text ()
findPkh = do
    pkh   <- pubKeyHash <$> Contract.ownPubKey
    logInfo @String $ printf "pubkeyhash is %s" (show pkh)
    tell $ Last $ Just pkh

findSale :: Sale -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut))
findSale sale = do
    utxos <- utxosAt $ OnChain.paarkaAddress sale
    return $ do
        (oref, o) <- find f $ Map.toList utxos
        return (oref, o)
  where
    f :: (TxOutRef, ChainIndexTxOut) -> Bool
    f (_, o) = assetClassValueOf (txOutValue $ toTxOut o) (assetClass (currency sale) (token sale)) == 1

buy :: Sale -> Integer -> PubKeyHash -> Contract w s Text ()
buy sale amount buyer = do
    m <- findSale sale
    case m of
        Nothing      -> Contract.logInfo @String $ printf "sale not found"
        Just(oref, o)-> do
            let tn = "PaarkaCoin"
                paarka  = Value.singleton paarkaSymbol tn amount
                val     = Value.singleton (nftTokenSymbol sale) (token sale) 1
                lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                          Constraints.otherScript (OnChain.validator sale) <>
                          Constraints.mintingPolicy paarkaPolicy <>
                          Constraints.mintingPolicy (nftTokenPolicy sale)
                tx      = Constraints.mustMintValue paarka <>
                          Constraints.mustPayToPubKey (ownerPkh sale) paarka <>
                          Constraints.mustMintValue val <>
                          Constraints.mustPayToPubKey buyer val <>
                          Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Buy buyer) <>
                          Constraints.mustPayToOtherScript (OnChain.valHash sale) (Datum $ PlutusTx.toBuiltinData ()) (txOutValue $ toTxOut o)
            ledgerTx <- submitTxConstraintsWith @Paarka lookups tx
            awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "minted %s tokens" (show paarka)
            logInfo @String $ printf "purchase done %s with amount" (show sale)

-- | Endpoints

type StartSaleSchema = Endpoint "start" SaleParams

startSaleEndpoint :: Contract (Last Sale) StartSaleSchema Text ()
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
