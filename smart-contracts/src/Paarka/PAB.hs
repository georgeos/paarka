{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}

module Paarka.PAB
    ( PaarkaContracts (..)
    ) where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import           GHC.Generics              (Generic)
import           Ledger                    (CurrencySymbol, TokenName, PubKeyHash)

data PaarkaContracts = StartSale
                     | Buy
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance Pretty PaarkaContracts where
    pretty = viaShow
