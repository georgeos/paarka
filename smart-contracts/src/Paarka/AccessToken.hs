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

module Paarka.AccessToken (
    nftTokenSymbol,
    nftTokenPolicy,
) where

import           Paarka.Utils           (paarkaPkh, Sale(..))
import           Control.Monad          hiding (fmap)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (Show (..), String)
import           Text.Printf            (printf)

-- | Onchain code

-- | Minting policy

{-# INLINABLE mintPolicy #-}
mintPolicy :: Sale -> PubKeyHash -> () -> ScriptContext -> Bool
mintPolicy sale pkhOwner _ ctx =
    case mintedValue of
        (cs, tn, amount)    ->
            traceIfFalse "Wrong currency symbol"         (cs == ownCurrencySymbol ctx) &&
            traceIfFalse "Wrong token name"              (tn == token sale) &&
            traceIfFalse "Wrong number of tokens minted" (amount == 1) &&
            traceIfFalse "Not signed by Paarka" checkSignature
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        mintedValue :: (CurrencySymbol, TokenName, Integer)
        mintedValue = head [ (cs, tn, amount) | (cs, tn, amount) <- flattenValue (txInfoMint info), cs == ownCurrencySymbol ctx ]

        checkSignature :: Bool
        checkSignature = txSignedBy info pkhOwner

nftTokenPolicy :: Sale -> Scripts.MintingPolicy
nftTokenPolicy sale = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \sale' paarkaPkh' -> Scripts.wrapMintingPolicy $ mintPolicy sale' paarkaPkh' ||])
    `PlutusTx.applyCode` PlutusTx.liftCode sale
    `PlutusTx.applyCode` PlutusTx.liftCode paarkaPkh

nftTokenSymbol :: Sale -> CurrencySymbol
nftTokenSymbol = scriptCurrencySymbol . nftTokenPolicy

-- | Offchain code
-- Testing purposes
type MintAccessTokenSchema = Endpoint "mint" Sale

mint :: Sale   -> Contract w MintAccessTokenSchema Text ()
mint sale = do
    let val     = Value.singleton (nftTokenSymbol sale) (token sale) 1
        lookups = Constraints.mintingPolicy (nftTokenPolicy sale)
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "minted 1 %s" (show sale)

endpoints :: Contract () MintAccessTokenSchema Text ()
endpoints = forever
        $ handleError logError
        $ awaitPromise mint'
  where
    mint' = endpoint @"mint" mint
