{-# LANGUAGE OverloadedStrings #-}
-- | A decentralized exchange for arbitrary token pairs following the
-- [Uniswap protocol](https://uniswap.org/whitepaper.pdf).
--
-- Details:
--
--  - 'OffChain' contains the instance endpoints and client functionality
--  - 'OnChain' contains the validation logic
module Paarka
  ( module OnChain
  , module OffChain
  , module Trace
  , module Types
  ) where

import Paarka.OffChain as OffChain
import Paarka.OnChain  as OnChain
import Paarka.Trace    as Trace
import Paarka.Types    as Types
