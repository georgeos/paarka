{-# LANGUAGE OverloadedStrings #-}
-- | Easy way to mint NFT for any type of media
-- [Paarka NFT]
--
-- Details:
--
--  - 'OffChain' contains the instance endpoints and client functionality
--  - 'OnChain' contains the validation logic
--  - 'Trace' contains the tests

module NFT
  ( module OnChain
  , module OffChain
  , module Trace
  ) where

import NFT.OffChain as OffChain
import NFT.OnChain  as OnChain
import NFT.Trace    as Trace
