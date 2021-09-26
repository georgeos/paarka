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
    runPaarka
) where

import           Paarka.Utils           (paarkaPkh, Sale(..))
import           Paarka.PaarkaCoin      (paarkaSymbol, paarkaPolicy)
import           Paarka.AccessToken     (nftTokenSymbol, nftTokenPolicy)
import           Control.Monad          hiding (fmap)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Default               (Default (..))
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
import           Prelude                (IO, Show (..), String, (<>))
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

-- | Onchain code

-- | Validator script

data PaarkaRedeemer = Buy PubKeyHash CurrencySymbol CurrencySymbol | SetPrice
    deriving Show

PlutusTx.unstableMakeIsData ''PaarkaRedeemer

-- | Validator
-- Validations TODO
-- Buy: not sure about Buy Redeemer, maybe there could be a security issue.
-- SetPrice:
-- Close:
-- - Owner has NFT in output
{-# INLINABLE paarkaValidator #-}
paarkaValidator :: Sale -> PubKeyHash -> () -> PaarkaRedeemer -> ScriptContext -> Bool
paarkaValidator sale pkhPaarka _ r ctx = traceIfFalse "Not signed by Paarka" checkSignature &&
    case r of
        Buy buyer paarka accessToken ->
            traceIfFalse "token missing from input"                 (hasNFT ownInput) &&
            traceIfFalse "token missing from output"                (hasNFT ownOutput) &&
            traceIfFalse "access token missing from buyer"          (accessTokenToBuyer buyer accessToken) &&
            traceIfFalse "access token missing from buyer"          (paarkaSpent paarka < paarkaProduced paarka)
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

        accessTokenToBuyer :: PubKeyHash -> CurrencySymbol -> Bool
        accessTokenToBuyer buyer accessToken = assetClassValueOf (valuePaidTo info buyer) (assetClass accessToken (token sale)) == 1

        paarkaSpent :: CurrencySymbol -> Integer
        paarkaSpent paarka =  assetClassValueOf (valueSpent info) (assetClass paarka (TokenName "PaarkaCoin"))

        paarkaProduced :: CurrencySymbol -> Integer
        paarkaProduced paarka =  assetClassValueOf (valueProduced info) (assetClass paarka (TokenName "PaarkaCoin"))

data Paarka
instance Scripts.ValidatorTypes Paarka where
    type instance DatumType Paarka = ()
    type instance RedeemerType Paarka = PaarkaRedeemer

typedValidator :: Sale -> Scripts.TypedValidator Paarka
typedValidator sale = Scripts.mkTypedValidator @Paarka
        ($$(PlutusTx.compile [|| paarkaValidator ||])
            `PlutusTx.applyCode` PlutusTx.liftCode sale
            `PlutusTx.applyCode` PlutusTx.liftCode paarkaPkh)
        $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @() @PaarkaRedeemer

validator :: Sale -> Validator
validator = Scripts.validatorScript . typedValidator

paarkaAddress :: Sale -> Ledger.Address
paarkaAddress = scriptAddress . validator

-- | Offchain code

-- | Contracts

data SaleParams = SaleParams {
     sCurrency :: !CurrencySymbol
    ,sToken    :: !TokenName
} deriving (Show, Generic, FromJSON, ToJSON)

startSale :: forall w s. SaleParams -> Contract w s Text ()
startSale sp = do
    pkh   <- pubKeyHash <$> Contract.ownPubKey
    let v = Value.singleton (sCurrency sp) (sToken sp) 1
        tx = mustPayToTheScript () v
        sale = Sale {
                owner = pkh,
                currency = sCurrency sp,
                token = sToken sp
            }
    ledgerTx <- submitTxConstraints (typedValidator sale) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "sale started for token %s" (show $ sToken sp)

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
    m   <- findSale sale
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
                          Constraints.mustPayToPubKey (owner sale) paarka <>
                          Constraints.mustMintValue val <>
                          Constraints.mustPayToPubKey buyer val <>
                          Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Buy buyer paarkaSymbol (nftTokenSymbol sale)) <>
                          Constraints.mustPayToOtherScript (valHash sale) (Datum $ PlutusTx.toBuiltinData ()) (txOutValue $ toTxOut o)
            ledgerTx <- submitTxConstraintsWith @Paarka lookups tx
            awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "minted %s tokens" (show paarka)
            logInfo @String $ printf "purchase done %s with amount" (show sale)

-- | Endpoints

-- | StartSale
type StartSaleSchema =
    Endpoint "start" (CurrencySymbol, TokenName)

startEndpoint :: Contract (Last SaleParams) StartSaleSchema Text ()
startEndpoint = forever
              $ handleError logError
              $ awaitPromise start'
    where
        start' = endpoint @"start" $ \(cs, tn) -> startSale SaleParams{ sCurrency= cs, sToken=tn }

-- | Sale
type SaleSchema =
    Endpoint "buy" (Integer, PubKeyHash)

saleEndpoints :: Sale -> Contract () SaleSchema Text ()
saleEndpoints sale = forever
            $ handleError logError
            $ awaitPromise buy'
    where
        buy' = endpoint @"buy" $ uncurry (buy sale)

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
    h2 <- activateContractWallet (Wallet 2) startEndpoint
    let saleOwner = pubKeyHash $ walletPubKey $ Wallet 2
        buyer3 = pubKeyHash $ walletPubKey $ Wallet 3
        buyer4 = pubKeyHash $ walletPubKey $ Wallet 4
    callEndpoint @"start" h2 (csNFT, tnNFT)
    void $ Emulator.waitNSlots 2
    h1 <- activateContractWallet (Wallet 1) $ saleEndpoints Sale{ currency=csNFT, token=tnNFT, owner=saleOwner }
    callEndpoint @"buy" h1 (10, buyer3)
    void $ Emulator.waitNSlots 1
    callEndpoint @"buy" h1 (10, buyer4)
    void $ Emulator.waitNSlots 1

runPaarka :: IO ()
runPaarka = runEmulatorTraceIO' def emCfg tracePaarka


runPaarkaPab :: OracleParams -> Contract (Last Oracle) OracleSchema DataText.Text ()
runPaarkaPab op = do
    oracle <- startOracle op
    tell $ Last $ Just oracle
    go oracle
  where
    go :: Oracle -> Contract (Last Oracle) OracleSchema DataText.Text a
    go oracle = do
        x <- endpoint @"update"
        updateOracle oracle x
        go oracle
