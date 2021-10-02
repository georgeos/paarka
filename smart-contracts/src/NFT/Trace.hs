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

module NFT.Trace (
    nftTrace
) where

import           Control.Monad          hiding (fmap)
import           Plutus.Trace.Emulator  as Emulator
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Prelude                (IO)
import           Wallet.Emulator.Wallet

import           NFT.OffChain           (nftEndpoint)

nftTrace :: IO ()
nftTrace = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (Wallet 1) nftEndpoint
    h2 <- activateContractWallet (Wallet 2) nftEndpoint
    callEndpoint @"mint-nft" h1 tn
    callEndpoint @"mint-nft" h2 tn
    void $ Emulator.waitNSlots 1
