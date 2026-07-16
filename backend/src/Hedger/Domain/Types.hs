module Hedger.Domain.Types (
    Bootstrap (..),
    Health (..),
)
where

import Data.Aeson qualified as Aeson
import Data.Aeson.TH qualified as Aeson
import Data.Text (Text)

data Health = Health
    { status :: Text
    }
    deriving (Eq, Show)

Aeson.deriveJSON Aeson.defaultOptions ''Health

data Bootstrap = Bootstrap
    { apiVersion :: Text
    , appName :: Text
    , fiscalYearStartMonth :: Int
    , defaultValuationBasis :: Text
    }
    deriving (Eq, Show)

Aeson.deriveJSON Aeson.defaultOptions ''Bootstrap
