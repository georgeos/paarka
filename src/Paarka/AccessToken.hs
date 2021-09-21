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

module Paarka.AccessToken where

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
mintPolicy :: BuiltinByteString -> () -> ScriptContext -> Bool
mintPolicy nftHash _ ctx =
    case mintedValue of
        (cs, tn, amount)    ->
            traceIfFalse "Wrong currency symbol"         (cs == ownCurrencySymbol ctx) &&
            traceIfFalse "Wrong token name"              (tn == TokenName nftHash) &&
            traceIfFalse "Wrong number of tokens minted" (amount == 1)
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        mintedValue :: (CurrencySymbol, TokenName, Integer)
        mintedValue = head [ (cs, tn, amount) | (cs, tn, amount) <- flattenValue (txInfoMint info), cs == ownCurrencySymbol ctx ]

nftTokenPolicy :: BuiltinByteString -> Scripts.MintingPolicy
nftTokenPolicy nftHash = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mintPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode nftHash

nftTokenSymbol :: BuiltinByteString -> CurrencySymbol
nftTokenSymbol = scriptCurrencySymbol . nftTokenPolicy

-- | Offchain code

type MintAccessTokenSchema = Endpoint "mint" BuiltinByteString

mint :: BuiltinByteString   -> Contract w MintAccessTokenSchema Text ()
mint nftHash = do
    let val     = Value.singleton (nftTokenSymbol nftHash) (TokenName nftHash) 1
        lookups = Constraints.mintingPolicy (nftTokenPolicy nftHash)
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "minted 1 %s" (show nftHash)

endpoints :: Contract () MintAccessTokenSchema Text ()
endpoints = forever
        $ handleError logError
        $ awaitPromise mint'
  where
    mint' = endpoint @"mint" mint

-- | Test

testAccessToken :: IO ()
testAccessToken = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"mint" h1 "MyNFT"
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h2 "OtherNFT"
    void $ Emulator.waitNSlots 1
