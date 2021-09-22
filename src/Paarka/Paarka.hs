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

module Paarka.Paarka where

import           Paarka.Utils           (paarkaPkh)
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

{-# INLINABLE mkValidator #-}
mkValidator :: Sale -> PubKeyHash -> () -> PaarkaRedeemer -> ScriptContext -> Bool
mkValidator _ pkhPaarka _ r ctx = traceIfFalse "Not signed by Paarka" checkSignature &&
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
        ($$(PlutusTx.compile [|| mkValidator ||])
            `PlutusTx.applyCode` PlutusTx.liftCode sale
            `PlutusTx.applyCode` PlutusTx.liftCode paarkaPkh)
        $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @() @PaarkaRedeemer

validator :: Sale -> Validator
validator = Scripts.validatorScript . typedValidator

valHash :: Sale -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedValidator

scrAddress :: Sale -> Ledger.Address
scrAddress = scriptAddress . validator

-- | Offchain code

-- | Contract

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

-- | Endpoint

type StartSaleSchema =
        Endpoint "start"      (CurrencySymbol, TokenName)

startEndpoint :: Contract (Last SaleParams) StartSaleSchema Text ()
startEndpoint = forever
              $ handleError logError
              $ awaitPromise start'
    where 
        start' = endpoint @"start" $ \(cs, tn) -> startSale SaleParams{ sCurrency= cs, sToken=tn }

-- | Test

csNFT :: CurrencySymbol
csNFT = "aa"

tnNFT :: TokenName
tnNFT = "A"

nft :: AssetClass
nft = AssetClass (csNFT, tnNFT)

runPaarka :: IO ()
runPaarka = runEmulatorTraceIO' def emCfg tracePaarka

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(Wallet w, v) | w <- [1 .. 3]]) def def
  where
    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000 <> assetClassValue nft 1

tracePaarka :: EmulatorTrace ()
tracePaarka = do
    h2 <- activateContractWallet (Wallet 2) startEndpoint
    callEndpoint @"start" h2 (csNFT, tnNFT)
    void $ Emulator.waitNSlots 1