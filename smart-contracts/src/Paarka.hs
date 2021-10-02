{-# LANGUAGE OverloadedStrings #-}
-- | A decentralized platform for media
-- [Paarka]
--
-- Details:
--
--  - 'OffChain' contains the instance endpoints and client functionality
--  - 'OnChain' contains the validation logic
--  - 'Types' contains the common types for OnChain and OffChain
--  - 'Trace' contains the tests

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
