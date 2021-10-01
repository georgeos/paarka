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

module Paarka.PaarkaCoin (
    paarkaSymbol,
    paarkaPolicy,
    testPaarkaCoin
) where

import           Paarka.Utils           (paarkaPkh)
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

{-# INLINABLE paarkaMintPolicy #-}
paarkaMintPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
paarkaMintPolicy pkhOwner _ ctx = traceIfFalse "Not signed by Paarka"              checkSignature &&
    case mintedValue of
        (cs, tn, _)    ->
            traceIfFalse "Wrong currency symbol" (cs == ownCurrencySymbol ctx) &&
            traceIfFalse "Wrong token name"      (tn == TokenName "PaarkaCoin")
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    mintedValue :: (CurrencySymbol, TokenName, Integer)
    mintedValue = head [ (cs, tn, amount) | (cs, tn, amount) <- flattenValue (txInfoMint info), cs == ownCurrencySymbol ctx ]

    checkSignature :: Bool
    checkSignature = txSignedBy info pkhOwner

paarkaPolicy :: Scripts.MintingPolicy
paarkaPolicy = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . paarkaMintPolicy ||])
        `PlutusTx.applyCode` PlutusTx.liftCode paarkaPkh

paarkaSymbol ::  CurrencySymbol
paarkaSymbol = scriptCurrencySymbol paarkaPolicy

-- | Offchain code
-- Testing purposes
type MintPaarkaSchema = Endpoint "mintPaarka" Integer

mintPaarka :: Integer -> Contract w MintPaarkaSchema Text ()
mintPaarka amount = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    Contract.logInfo @String $ printf "PubKeyHash: %s" (show pkh)
    let paarkaCoin = "PaarkaCoin"
        val     = Value.singleton paarkaSymbol paarkaCoin amount
        lookups = Constraints.mintingPolicy paarkaPolicy
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "minted %s PaarkaCoin" (show val)

paarkaEndpoints :: Contract () MintPaarkaSchema Text ()
paarkaEndpoints = forever
        $ handleError logError
        $ awaitPromise mintPaarka'
  where
    mintPaarka' = endpoint @"mintPaarka" mintPaarka

-- | Test

testPaarkaCoin :: IO ()
testPaarkaCoin = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (Wallet 1) paarkaEndpoints
    h2 <- activateContractWallet (Wallet 2) paarkaEndpoints
    callEndpoint @"mintPaarka" h1 20
    void $ Emulator.waitNSlots 1
    callEndpoint @"mintPaarka" h2 10
    void $ Emulator.waitNSlots 1
