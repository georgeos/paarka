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

module NFT.OffChain (
    NFTSchema,
    nftEndpoint,
    mintNFT,
    curSymbol
) where

import           Control.Monad          hiding (fmap)
import qualified Data.Map               as Map
import           Data.Monoid            (Last(..))
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
-- import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Plutus.Contract        as Contract
-- import           Plutus.Trace.Emulator  as Emulator
-- import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Prelude                (Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
-- import           Wallet.Emulator.Wallet

import           NFT.OnChain            as OnChain

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ OnChain.nftMintingPolicy oref tn


type NFTSchema = Endpoint "mint-nft" TokenName

mintNFT :: TokenName -> Contract (Last (CurrencySymbol, TokenName)) NFTSchema Text ()
mintNFT tn = do
    pk    <- Contract.ownPubKey
    utxos <- utxosAt (pubKeyAddress pk)
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let myCurSymbol = curSymbol oref tn
                val         = Value.singleton myCurSymbol tn 1
                lookups     = Constraints.mintingPolicy (OnChain.nftMintingPolicy oref tn) <> Constraints.unspentOutputs utxos
                tx          = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)
            tell $ Last $ Just (myCurSymbol, tn)

nftEndpoint :: Contract (Last (CurrencySymbol, TokenName)) NFTSchema Text ()
nftEndpoint = forever
     $ handleError logError
     $ awaitPromise mint'
     where
     mint' = endpoint @"mint-nft" $ \tn -> mintNFT tn
