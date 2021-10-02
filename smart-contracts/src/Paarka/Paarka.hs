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

module Paarka.Paarka (
    runPaarka,
    SaleParams(..),
    StartSaleSchema, BuySchema,
    startSale, buy,
    startSaleEndpoint, buyEndpoints
) where

import           Paarka.Utils           (paarkaPkh, Sale(..))
import           Paarka.PaarkaCoin      (paarkaSymbol, paarkaPolicy)
import           Paarka.AccessToken     (nftTokenSymbol, nftTokenPolicy)
import           Control.Monad          hiding (fmap)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Default           (Default (..))
import qualified Data.Map               as Map
import           Data.Monoid            (Last (..))
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Ada             as Ada
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Schema                 (ToSchema)

import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet
import           Prelude                (Semigroup (..), Show (..), String, (<>), IO)
import qualified Prelude

-- | Onchain code

-- | Validator script

data PaarkaRedeemer = Buy PubKeyHash | SetPrice
    deriving Show

PlutusTx.unstableMakeIsData ''PaarkaRedeemer

-- | Validator
-- Validations TODO
-- SetPrice:
-- Close:
-- - Owner has NFT in output
{-# INLINABLE paarkaValidator #-}
paarkaValidator :: CurrencySymbol -> CurrencySymbol -> Sale -> PubKeyHash -> () -> PaarkaRedeemer -> ScriptContext -> Bool
paarkaValidator paarka accessToken sale pkhPaarka _ r ctx = traceIfFalse "Not signed by Paarka" checkSignature &&
    case r of
        Buy buyer ->
            traceIfFalse "token missing from input"                 (hasNFT ownInput) &&
            traceIfFalse "token missing from output"                (hasNFT ownOutput) &&
            traceIfFalse "access token missing from buyer"          (accessTokenToBuyer buyer) &&
            traceIfFalse "access token missing from buyer"          (paarkaSpent < paarkaProduced)
        SetPrice -> traceIfFalse "Second version" True
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        checkSignature :: Bool
        checkSignature = txSignedBy info pkhPaarka

        ownInput :: TxOut
        ownInput = case findOwnInput ctx of
            Nothing -> traceError "sale input missing"
            Just i  -> txInInfoResolved i

        ownOutput :: TxOut
        ownOutput = case getContinuingOutputs ctx of
            [o] -> o
            _   -> traceError "missing one sale output"

        hasNFT :: TxOut -> Bool
        hasNFT txOut = assetClassValueOf (txOutValue txOut ) (assetClass (currency sale) (token sale)) == 1

        accessTokenToBuyer :: PubKeyHash -> Bool
        accessTokenToBuyer buyer = assetClassValueOf (valuePaidTo info buyer) (assetClass accessToken (token sale)) == 1

        paarkaSpent :: Integer
        paarkaSpent = assetClassValueOf (valueSpent info) (assetClass paarka (TokenName "PaarkaCoin"))

        paarkaProduced :: Integer
        paarkaProduced = assetClassValueOf (valueProduced info) (assetClass paarka (TokenName "PaarkaCoin"))

data Paarka
instance Scripts.ValidatorTypes Paarka where
    type instance DatumType Paarka = ()
    type instance RedeemerType Paarka = PaarkaRedeemer

typedValidator :: Sale -> Scripts.TypedValidator Paarka
typedValidator sale = Scripts.mkTypedValidator @Paarka
        ($$(PlutusTx.compile [|| paarkaValidator ||])
            `PlutusTx.applyCode` PlutusTx.liftCode paarkaSymbol
            `PlutusTx.applyCode` PlutusTx.liftCode (nftTokenSymbol sale)
            `PlutusTx.applyCode` PlutusTx.liftCode sale
            `PlutusTx.applyCode` PlutusTx.liftCode paarkaPkh
        )
        $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @() @PaarkaRedeemer

validator :: Sale -> Validator
validator = Scripts.validatorScript . typedValidator

valHash :: Sale -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedValidator

paarkaAddress :: Sale -> Ledger.Address
paarkaAddress = scriptAddress . validator

-- | Offchain code

-- | Contracts

data SaleParams = SaleParams {
     sCurrency :: !CurrencySymbol
    ,sToken    :: !TokenName
} deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

data BuyParams = BuyParams {
     nftSale  :: !Sale
    ,amt      :: !Integer
    ,buyerPkh :: !PubKeyHash
} deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

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
    ledgerTx <- submitTxConstraints (typedValidator sale) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "sale started for token %s" (show sale)
    return sale

findPkh :: forall w s. Contract w s Text PubKeyHash
findPkh = do
    pkh   <- pubKeyHash <$> Contract.ownPubKey
    logInfo @String $ printf "pubkeyhash is %s" (show pkh)
    return pkh

findSale :: Sale -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut))
findSale sale = do
    utxos <- utxosAt $ paarkaAddress sale
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
                          Constraints.otherScript (validator sale) <>
                          Constraints.mintingPolicy paarkaPolicy <>
                          Constraints.mintingPolicy (nftTokenPolicy sale)
                tx      = Constraints.mustMintValue paarka <>
                          Constraints.mustPayToPubKey (ownerPkh sale) paarka <>
                          Constraints.mustMintValue val <>
                          Constraints.mustPayToPubKey buyer val <>
                          Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Buy buyer) <>
                          Constraints.mustPayToOtherScript (valHash sale) (Datum $ PlutusTx.toBuiltinData ()) (txOutValue $ toTxOut o)
            ledgerTx <- submitTxConstraintsWith @Paarka lookups tx
            awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "minted %s tokens" (show paarka)
            logInfo @String $ printf "purchase done %s with amount" (show sale)

-- | Endpoints

-- | StartSale
type StartSaleSchema = Endpoint "start" SaleParams

-- | We should find the way to return Sale, coming from StartSale Contract
-- There is an example in Swap.hs from lesson 6 -> swap Contract -> funds Contract but doesn't work here
startSaleEndpoint :: Contract (Last Sale) StartSaleSchema Text ()
startSaleEndpoint = forever
              $ handleError logError
              $ awaitPromise start' >>= tell . Last . Just
    where
        start' = endpoint @"start" $ \sp -> startSale sp

-- | BuySchema
-- | find-pkh Temporal function to find the PubKeyHash. Should return a PubKeyHash in the HTTP request
type BuySchema = Endpoint "buy" BuyParams

buyEndpoints :: Contract () BuySchema Text ()
buyEndpoints = forever
            $ handleError logError
            $ awaitPromise buy'
    where
        buy' = endpoint @"buy" $ \bp -> buy (nftSale bp) (amt bp) (buyerPkh bp)

-- | BuySchema
-- | find-pkh Temporal function to find the PubKeyHash. Should return a PubKeyHash in the HTTP request
type FindSchema = Endpoint "find-pkh" ()

findEndpoints :: Contract (Last PubKeyHash) FindSchema Text ()
findEndpoints = forever
            $ handleError logError
            $ awaitPromise findPkh'
    where
        findPkh' :: Contract (Last PubKeyHash) FindSchema Text ()
        findPkh' = do
            endpoint @"find-pkh"
            pkh <- findPkh
            tell $ Last $ Just pkh

-- | Trace

csNFT :: CurrencySymbol
csNFT = "aa"

tnNFT :: TokenName
tnNFT = "A"

-- | AssetClass used as NFT to sale
nft :: AssetClass
nft = AssetClass (csNFT, tnNFT)

-- | EmulatorConfig considering only 3 wallets.
-- Wallet 1 is Paarka, Wallet 2 is nft owner and Wallet 3 and 4 is buyer.
emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [( Wallet w,
        case w of
            1   -> ada 1_000_000_000
            2   -> v <> ada 1_000_000_000
            _   -> ada 0
    ) | w <- [1 .. 10]]) def def
  where
    ada :: Integer -> Value
    ada val = Ada.lovelaceValueOf val

    v :: Value
    v = assetClassValue nft 1

tracePaarka :: EmulatorTrace ()
tracePaarka = do
    h2 <- activateContractWallet (Wallet 2) startSaleEndpoint
    let saleOwner = pubKeyHash $ walletPubKey $ Wallet 2
        buyer3 = pubKeyHash $ walletPubKey $ Wallet 3
        buyer4 = pubKeyHash $ walletPubKey $ Wallet 4
    callEndpoint @"start" h2 SaleParams{sCurrency=csNFT, sToken=tnNFT}
    void $ Emulator.waitNSlots 2
    h1 <- activateContractWallet (Wallet 1) $ buyEndpoints
    callEndpoint @"buy" h1  BuyParams{ nftSale=Sale{ currency=csNFT, token=tnNFT, ownerPkh=saleOwner }, amt=10, buyerPkh=buyer3}
    void $ Emulator.waitNSlots 1
    callEndpoint @"buy" h1  BuyParams{ nftSale=Sale{ currency=csNFT, token=tnNFT, ownerPkh=saleOwner }, amt=10, buyerPkh=buyer4}
    void $ Emulator.waitNSlots 1

runPaarka :: IO ()
runPaarka = runEmulatorTraceIO' def emCfg tracePaarka
