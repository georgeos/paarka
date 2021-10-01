{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}

module Paarka.PAB
    ( PaarkaContracts (..)
    ) where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Text.Prettyprint.Doc (Pretty (..), viaShow)
import           GHC.Generics              (Generic)
import qualified Ledger
import qualified Paarka.Paarka             as Paarka
import           Paarka.Utils              (Sale(..))

data PaarkaContracts = StartSale Paarka.SaleParams
                     | Buy Sale Integer Ledger.PubKeyHash
    deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance Pretty PaarkaContracts where
    pretty = viaShow
