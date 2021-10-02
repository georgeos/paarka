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

module Paarka.OnChain (
    paarkaValidator,
    typedValidator,
    validator,
    valHash,
    paarkaAddress
) where

import           Paarka.Utils           (paarkaPkh, Sale(..))
import           Ledger                 hiding (mint, singleton)
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)

import           Paarka.PaarkaCoin      (paarkaSymbol)
import           Paarka.AccessToken     (nftTokenSymbol)
import           Paarka.Types

-- | Validator script

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
