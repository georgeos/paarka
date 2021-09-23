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

import           Paarka.Utils           (paarkaPkh)
import           Paarka.PaarkaCoin      (paarkaSymbol, paarkaPolicy)
import           Control.Monad          hiding (fmap)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Default               (Default (..))
import qualified Data.Map               as Map
import           Data.Monoid            (Last (..))
import           Data.Text              (Text)
import           Data.Void              ( Void )
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
import qualified Prelude
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

-- | Onchain code

data Sale = Sale
    { owner     :: !PubKeyHash
    , currency  :: !CurrencySymbol
    , token     :: !TokenName
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Sale

-- | Validator script

data PaarkaRedeemer = Buy | SetPrice
    deriving Show

PlutusTx.unstableMakeIsData ''PaarkaRedeemer

-- | Validator
-- TODO
{-# INLINABLE paarkaValidator #-}
paarkaValidator :: Sale -> PubKeyHash -> () -> PaarkaRedeemer -> ScriptContext -> Bool
paarkaValidator _ pkhPaarka _ r ctx = traceIfFalse "Not signed by Paarka" checkSignature &&
    case r of
        Buy      -> traceIfFalse "First version" True
        SetPrice -> traceIfFalse "Second version" True
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        checkSignature :: Bool
        checkSignature = txSignedBy info pkhPaarka

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

valHash :: Sale -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedValidator

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

buy :: Sale -> Integer -> PubKeyHash -> Contract w s Text ()
buy sale amount _ = do
    pkh   <- pubKeyHash <$> Contract.ownPubKey
    let tn = "PaarkaCoin"
        val = Value.singleton (paarkaSymbol pkh) tn amount
        lookups = Constraints.mintingPolicy (paarkaPolicy pkh)
        tx      = Constraints.mustMintValue val <>
                  Constraints.mustPayToPubKey (owner sale) val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "minted %s tokens" (show val)
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
-- Wallet 1 is Paarka, Wallet 2 is nft owner and Wallet 3 is buyer.
emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [( Wallet w, if w == 2 then v <> a else a ) | w <- [1 .. 3]]) def def
  where
    a :: Value
    a = Ada.lovelaceValueOf 1_000_000_000

    v :: Value
    v = assetClassValue nft 1

tracePaarka :: EmulatorTrace ()
tracePaarka = do
    h2 <- activateContractWallet (Wallet 2) startEndpoint
    let saleOwner = pubKeyHash $ walletPubKey $ Wallet 2
        buyer = pubKeyHash $ walletPubKey $ Wallet 3
    callEndpoint @"start" h2 (csNFT, tnNFT)
    void $ Emulator.waitNSlots 2
    h1 <- activateContractWallet (Wallet 1) $ saleEndpoints Sale{ currency=csNFT, token=tnNFT, owner=saleOwner }
    callEndpoint @"buy" h1 (10, buyer)
    void $ Emulator.waitNSlots 1

runPaarka :: IO ()
runPaarka = runEmulatorTraceIO' def emCfg tracePaarka