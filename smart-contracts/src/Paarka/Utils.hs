{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Paarka.Utils (
    SaleParams(..), BuyParams(..),
    paarkaPkh,
    salePrice
) where

import           Data.Aeson             (FromJSON, ToJSON)
import           GHC.Generics           (Generic)
import           Ledger                 hiding (mint, singleton)
import qualified PlutusTx
import           Prelude                (Show (..), Integer, Maybe)
import qualified Prelude
import           Schema                 (ToSchema)

data SaleParams = SaleParams
    { ownerPkh  :: ![PubKeyHash]
    , share     :: ![Integer]
    , currency  :: !CurrencySymbol
    , token     :: !TokenName
    , price     :: !Integer
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''SaleParams

data BuyParams = BuyParams {
     nftSale  :: !SaleParams
    ,buyerPkh :: !PubKeyHash
} deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''BuyParams

paarkaPkh :: PubKeyHash
paarkaPkh = "35dedd2982a03cf39e7dce03c839994ffdec2ec6b04f1cf2d40e61a3" :: PubKeyHash

salePrice :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Integer
salePrice o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d