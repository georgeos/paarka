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

module Paarka.PaarkaCoin where

import           Control.Monad          hiding (fmap)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

-- | Onchain code

-- | Minting policy

{-# INLINABLE mintPolicy #-}
mintPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mintPolicy pkh _ ctx =  traceIfFalse "Wrong token minted" checkMintedCoin
                    &&  traceIfFalse "Not signed by Paarka" checkSignature
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkMintedCoin :: Bool
    checkMintedCoin = case flattenValue (txInfoMint info) of
        [(cs, tn, _)] -> cs  == ownCurrencySymbol ctx && tn == TokenName "PaarkaCoin"
        _                -> False

    checkSignature :: Bool
    checkSignature = txSignedBy info pkh
                -- &&   pkh == PubKeyHash "35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3"

paarkaPolicy :: PubKeyHash -> Scripts.MintingPolicy
paarkaPolicy pkh = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mintPolicy ||]) `PlutusTx.applyCode` PlutusTx.liftCode pkh

paarkaSymbol :: PubKeyHash -> CurrencySymbol
paarkaSymbol = scriptCurrencySymbol . paarkaPolicy

-- | Offchain code

type MintPaarkaSchema = Endpoint "mint" Integer

mint :: Integer -> Contract w MintPaarkaSchema Text ()
mint amount = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    Contract.logInfo @String $ printf "PubKeyHash: %s" (show pkh)
    let paarkaCoin = "PaarkaCoin"
        val     = Value.singleton (paarkaSymbol pkh) paarkaCoin amount
        lookups = Constraints.mintingPolicy (paarkaPolicy pkh)
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "minted %s PaarkaCoin" (show val)

endpoints :: Contract () MintPaarkaSchema Text ()
endpoints = forever
        $ handleError logError
        $ awaitPromise mint'
  where
    mint' = endpoint @"mint" mint

-- | Test

testPaarkaCoin :: IO ()
testPaarkaCoin = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"mint" h1 20
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h2 10
    void $ Emulator.waitNSlots 1
