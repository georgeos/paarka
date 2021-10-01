{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Paarka.Utils (
    Sale(..),
    paarkaPkh
) where

import           Data.Aeson             (FromJSON, ToJSON)
import           GHC.Generics           (Generic)
import           Ledger                 (PubKeyHash)
import           Ledger.Value           as Value
import qualified PlutusTx
import           Prelude                (Show (..))
import qualified Prelude
import           Schema                 (ToSchema)

data Sale = Sale
    { owner     :: !PubKeyHash
    , currency  :: !CurrencySymbol
    , token     :: !TokenName
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Sale

paarkaPkh :: PubKeyHash
paarkaPkh = "35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3" :: PubKeyHash
