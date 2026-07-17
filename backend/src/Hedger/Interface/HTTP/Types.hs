module Hedger.Interface.HTTP.Types (
    AmountDto (..),
    Bootstrap (..),
    CostDto (..),
    Health (..),
    PostingDto (..),
    ResponseMetadata (..),
    TagDto (..),
    TransactionDto (..),
    TransactionPresentationDto (..),
    TransactionsResponse (..),
)
where

import Data.Aeson qualified as Aeson
import Data.Aeson.TH qualified as Aeson
import Data.Text (Text)
import Data.Time (Day)

data Health = Health
    { status :: Text
    }
    deriving (Eq, Show)

Aeson.deriveJSON Aeson.defaultOptions ''Health

data ResponseMetadata = ResponseMetadata
    { revision :: Text
    , freshness :: Text
    , warnings :: [Text]
    }
    deriving (Eq, Show)

Aeson.deriveJSON Aeson.defaultOptions ''ResponseMetadata

data Bootstrap = Bootstrap
    { apiVersion :: Text
    , appName :: Text
    , fiscalYearStartMonth :: Int
    , metadata :: ResponseMetadata
    }
    deriving (Eq, Show)

Aeson.deriveJSON Aeson.defaultOptions ''Bootstrap

data TagDto = TagDto
    { name :: Text
    , value :: Text
    }
    deriving (Eq, Show)

Aeson.deriveJSON Aeson.defaultOptions ''TagDto

data CostDto = CostDto
    { costType :: Text
    , commodity :: Text
    , quantity :: Text
    }
    deriving (Eq, Show)

Aeson.deriveJSON Aeson.defaultOptions ''CostDto

data AmountDto = AmountDto
    { commodity :: Text
    , quantity :: Text
    , cost :: Maybe CostDto
    }
    deriving (Eq, Show)

Aeson.deriveJSON Aeson.defaultOptions ''AmountDto

data PostingDto = PostingDto
    { account :: Text
    , status :: Text
    , comment :: Maybe Text
    , tags :: [TagDto]
    , amounts :: [AmountDto]
    }
    deriving (Eq, Show)

Aeson.deriveJSON Aeson.defaultOptions ''PostingDto

data TransactionPresentationDto = TransactionPresentationDto
    { classification :: Text
    }
    deriving (Eq, Show)

Aeson.deriveJSON Aeson.defaultOptions ''TransactionPresentationDto

data TransactionDto = TransactionDto
    { primaryDate :: Day
    , secondaryDate :: Maybe Day
    , status :: Text
    , code :: Maybe Text
    , description :: Text
    , comment :: Maybe Text
    , tags :: [TagDto]
    , postings :: [PostingDto]
    , presentation :: TransactionPresentationDto
    }
    deriving (Eq, Show)

Aeson.deriveJSON Aeson.defaultOptions ''TransactionDto

data TransactionsResponse = TransactionsResponse
    { metadata :: ResponseMetadata
    , transactions :: [TransactionDto]
    , nextCursor :: Maybe Text
    }
    deriving (Eq, Show)

Aeson.deriveJSON Aeson.defaultOptions ''TransactionsResponse
