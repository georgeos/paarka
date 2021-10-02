{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module NFT.OnChain (
    nftPolicyValidator,
    nftMintingPolicy
) where

-- import           Control.Monad          hiding (fmap)
-- import qualified Data.Map               as Map
-- import           Data.Text              (Text)
-- import           Data.Void              (Void)
import           Ledger                 hiding (mint, singleton)
-- import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
-- import           Plutus.Contract        as Contract
-- import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
-- import           Prelude                (IO, Semigroup (..), Show (..), String)
-- import           Text.Printf            (printf)
-- import           Wallet.Emulator.Wallet

{-# INLINABLE nftPolicyValidator #-}
nftPolicyValidator :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
nftPolicyValidator oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                          traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tn && amt == 1
        _                -> False

nftMintingPolicy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
nftMintingPolicy oref tn = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ nftPolicyValidator oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn

