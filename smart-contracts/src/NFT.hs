{-# LANGUAGE OverloadedStrings #-}
-- | A decentralized exchange for arbitrary token pairs following the
-- [Uniswap protocol](https://uniswap.org/whitepaper.pdf).
--
-- Details:
--
--  - 'OffChain' contains the instance endpoints and client functionality
--  - 'OnChain' contains the validation logic
module NFT
  ( module OnChain
  , module OffChain
  , module Trace
  ) where

import NFT.OffChain as OffChain
import NFT.OnChain  as OnChain
import NFT.Trace    as Trace
