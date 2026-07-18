module Hedger.Interface.HTTP.Types (
    AmountDto (..),
    AccountNodeDto (..),
    AccountValuationDto (..),
    AccountsTreeResponse (..),
    Bootstrap (..),
    CostDto (..),
    CommodityQuantity (..),
    Health (..),
    PostingDto (..),
    RegisterEntryDto (..),
    RegisterValueDto (..),
    ResponseMetadata (..),
    TagDto (..),
    TransactionDto (..),
    TransactionPresentationDto (..),
    TransactionsResponse (..),
    AccountRegisterResponse (..),
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

data CommodityQuantity = CommodityQuantity
    { commodity :: Text
    , quantity :: Text
    }
    deriving (Eq, Show)

Aeson.deriveJSON Aeson.defaultOptions ''CommodityQuantity

data RegisterValueDto = RegisterValueDto
    { raw :: [CommodityQuantity]
    , display :: [CommodityQuantity]
    , completeness :: Text
    }
    deriving (Eq, Show)

Aeson.deriveJSON Aeson.defaultOptions ''RegisterValueDto

data RegisterEntryDto = RegisterEntryDto
    { date :: Day
    , description :: Text
    , account :: Text
    , amount :: RegisterValueDto
    , balance :: RegisterValueDto
    }
    deriving (Eq, Show)

Aeson.deriveJSON Aeson.defaultOptions ''RegisterEntryDto

data AccountRegisterResponse = AccountRegisterResponse
    { metadata :: ResponseMetadata
    , displayCommodity :: Text
    , entries :: [RegisterEntryDto]
    , nextCursor :: Maybe Text
    }
    deriving (Eq, Show)

Aeson.deriveJSON Aeson.defaultOptions ''AccountRegisterResponse

data AccountValuationDto = AccountValuationDto
    { asOf :: Day
    , commodity :: Text
    , directBalances :: [CommodityQuantity]
    , subtreeBalances :: [CommodityQuantity]
    , completeness :: Text
    }
    deriving (Eq, Show)

Aeson.deriveJSON Aeson.defaultOptions ''AccountValuationDto

data AccountNodeDto = AccountNodeDto
    { path :: Text
    , name :: Text
    , accountType :: Maybe Text
    , hasChildren :: Bool
    , directBalances :: [CommodityQuantity]
    , subtreeBalances :: [CommodityQuantity]
    , valuation :: AccountValuationDto
    , children :: [AccountNodeDto]
    }
    deriving (Eq, Show)

Aeson.deriveJSON Aeson.defaultOptions ''AccountNodeDto

data AccountsTreeResponse = AccountsTreeResponse
    { metadata :: ResponseMetadata
    , displayCommodity :: Text
    , accounts :: [AccountNodeDto]
    }
    deriving (Eq, Show)

Aeson.deriveJSON Aeson.defaultOptions ''AccountsTreeResponse
