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

module Paarka.Trace (
    runPaarka
) where

import           Control.Monad          hiding (fmap)
import           Data.Default           (Default (..))
import qualified Data.Map               as Map
import           Ledger                 hiding (mint, singleton)
import           Ledger.Ada             as Ada
import           Ledger.Value           as Value
import           Plutus.Trace.Emulator  as Emulator
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)

import           Wallet.Emulator.Wallet
import           Prelude                (Semigroup (..), (<>), IO)

import           Paarka.Utils           (SaleParams(..), BuyParams(..))
import           Paarka.OffChain        (startSaleEndpoint, buyEndpoints)

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
    h2 <- activateContractWallet (Wallet 2) startSaleEndpoint
    let saleOwner1 = pubKeyHash $ walletPubKey $ Wallet 2
        saleOwner2 = pubKeyHash $ walletPubKey $ Wallet 6
        saleOwner3 = pubKeyHash $ walletPubKey $ Wallet 7
        buyer3 = pubKeyHash $ walletPubKey $ Wallet 3
        buyer4 = pubKeyHash $ walletPubKey $ Wallet 4
        buyer5 = pubKeyHash $ walletPubKey $ Wallet 5
        sp = SaleParams{ownerPkh=[saleOwner1, saleOwner2, saleOwner3], share=[65, 25, 10], currency=csNFT, token=tnNFT, price=10}
    callEndpoint @"start" h2 sp
    void $ Emulator.waitNSlots 2
    h1 <- activateContractWallet (Wallet 1) buyEndpoints
    callEndpoint @"buy" h1  BuyParams{ nftSale=sp, buyerPkh=buyer3}
    void $ Emulator.waitNSlots 1
    callEndpoint @"buy" h1  BuyParams{ nftSale=sp, buyerPkh=buyer4}
    void $ Emulator.waitNSlots 1
    callEndpoint @"buy" h1  BuyParams{ nftSale=sp, buyerPkh=buyer5}
    void $ Emulator.waitNSlots 1

runPaarka :: IO ()
runPaarka = runEmulatorTraceIO' def emCfg tracePaarka
