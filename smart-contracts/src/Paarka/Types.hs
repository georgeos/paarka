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

module Paarka.Types where

import           Ledger                 hiding (mint, singleton)
import qualified Ledger.Typed.Scripts   as Scripts
import           Prelude                (Show (..), Integer)
import qualified PlutusTx

-- | Onchain code

-- | Validator script

data PaarkaRedeemer = Buy PubKeyHash | SetPrice
    deriving Show

PlutusTx.unstableMakeIsData ''PaarkaRedeemer

data Paarka
instance Scripts.ValidatorTypes Paarka where
    type instance DatumType Paarka = Integer
    type instance RedeemerType Paarka = PaarkaRedeemer
