module Hedger.Interface.HTTP.Endpoints.Transactions (
    TransactionsAPI,
    transactionsServer,
    toTransactions,
    AmountDto (..),
    CostDto (..),
    PostingDto (..),
    TagDto (..),
    TransactionDto (..),
    TransactionPresentationDto (..),
    TransactionsResponse (..),
    toTransactionDto,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)
import GHC.Generics (Generic)
import Servant

import Hledger.Data.Amount qualified as Hledger
import Hledger.Data.Types qualified as Hledger

import Hedger.Application.AppM (Env)
import Hedger.Application.Common (AccountTypeClassifier)
import Hedger.Application.TransactionQuery (TransactionPageRequest (..))
import Hedger.Application.Transactions (TransactionList (..), transactionList)
import Hedger.Domain.Transaction qualified as Domain
import Hedger.Interface.HTTP.Core.Common (
    ResponseMetadata (..),
    accountTypeText,
    parseLimit,
    runAction,
    servantError,
    toResponseMetadata,
 )

-- DTO declaration

data TagDto = TagDto
    { name :: Text
    , value :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data CostDto = CostDto
    { costType :: Text
    , commodity :: Text
    , quantity :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data AmountDto = AmountDto
    { commodity :: Text
    , quantity :: Text
    , cost :: Maybe CostDto
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data PostingDto = PostingDto
    { account :: Text
    , accountType :: Maybe Text
    , status :: Text
    , comment :: Maybe Text
    , tags :: [TagDto]
    , amounts :: [AmountDto]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data TransactionPresentationDto = TransactionPresentationDto
    { classification :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

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
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data TransactionsResponse = TransactionsResponse
    { metadata :: ResponseMetadata
    , transactions :: [TransactionDto]
    , nextCursor :: Maybe Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- API and server

type TransactionsAPI =
    "transactions"
        :> QueryParam "query" Text
        :> QueryParam "limit" Text
        :> QueryParam "cursor" Text
        :> Get '[JSON] TransactionsResponse

transactionsServer :: Env -> Maybe Text -> Server TransactionsAPI
transactionsServer env requestId query limit cursor =
    case parseLimit limit of
        Left err -> throwError (servantError requestId err)
        Right limitValue -> runAction env requestId $ do
            let request = TransactionPageRequest{query, limit = limitValue, cursor}
            toTransactionsResponse <$> transactionList request

-- Mapper: domain <-> DTO

toTransactionsResponse :: TransactionList -> TransactionsResponse
toTransactionsResponse value =
    TransactionsResponse
        { metadata = toResponseMetadata value.metadata
        , transactions = toTransactions value.classifyAccount value.transactions
        , nextCursor = value.nextCursor
        }

toTransactions :: AccountTypeClassifier -> [Hledger.Transaction] -> [TransactionDto]
toTransactions classify = fmap (toTransactionDto classify) . Domain.newestFirst

toTransactionDto :: AccountTypeClassifier -> Hledger.Transaction -> TransactionDto
toTransactionDto classify transaction =
    TransactionDto
        { primaryDate = transaction.tdate
        , secondaryDate = transaction.tdate2
        , status = statusText transaction.tstatus
        , code = nonEmpty transaction.tcode
        , description = transaction.tdescription
        , comment = nonEmpty transaction.tcomment
        , tags = projectTags transaction.ttags
        , postings = projectPosting classify <$> transaction.tpostings
        , presentation = presentationDto (Domain.presentationFor transaction)
        }

projectPosting :: AccountTypeClassifier -> Hledger.Posting -> PostingDto
projectPosting classify posting =
    PostingDto
        { account = posting.paccount
        , accountType = accountTypeText <$> classify posting.paccount
        , status = statusText posting.pstatus
        , comment = nonEmpty posting.pcomment
        , tags = projectTags posting.ptags
        , amounts = projectAmount <$> Hledger.amountsRaw posting.pamount
        }

projectAmount :: Hledger.Amount -> AmountDto
projectAmount amount =
    AmountDto
        { commodity = amount.acommodity
        , quantity = Text.pack (show amount.aquantity)
        , cost = projectCost <$> amount.acost
        }

projectCost :: Hledger.AmountCost -> CostDto
projectCost amountCost =
    case amountCost of
        Hledger.UnitCost amount -> costDto "unit" amount
        Hledger.TotalCost amount -> costDto "total" amount

costDto :: Text -> Hledger.Amount -> CostDto
costDto costType amount =
    CostDto
        { costType = costType
        , commodity = amount.acommodity
        , quantity = Text.pack (show amount.aquantity)
        }

projectTags :: [Hledger.Tag] -> [TagDto]
projectTags = fmap (\(name, value) -> TagDto{name = name, value = value})

statusText :: Hledger.Status -> Text
statusText status =
    case status of
        Hledger.Unmarked -> "unmarked"
        Hledger.Pending -> "pending"
        Hledger.Cleared -> "cleared"

nonEmpty :: Text -> Maybe Text
nonEmpty value
    | Text.null value = Nothing
    | otherwise = Just value

presentationDto :: Domain.TransactionPresentation -> TransactionPresentationDto
presentationDto presentation =
    TransactionPresentationDto
        { classification = classificationText presentation.classification
        }

classificationText :: Domain.TransactionClassification -> Text
classificationText classification =
    case classification of
        Domain.TransactionIncome -> "income"
        Domain.TransactionExpense -> "expense"
        Domain.TransactionTransfer -> "transfer"
        Domain.OpeningBalance -> "opening_balance"
        Domain.Borrowing -> "borrowing"
        Domain.Repayment -> "repayment"
        Domain.AssetAcquisition -> "asset_acquisition"
        Domain.AssetDisposal -> "asset_disposal"
        Domain.AssetReallocation -> "asset_reallocation"
        Domain.Unclassified -> "unclassified"
