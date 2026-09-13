module Hedger.Interface.HTTP.Core.Common (
    CommodityQuantity (..),
    ResponseMetadata (..),
    accountTypeText,
    commodityQuantities,
    decimalText,
    granularityText,
    investmentCompletenessText,
    parseGranularity,
    toResponseMetadata,
    valuationCompletenessText,
    runAction,
    servantError,
    effectiveRequestId,
    defaultPageLimit,
    maximumPageLimit,
    parseLimit,
    parseDay,
    parseCanonicalAccountPath,
    validateDateRange,
)
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Decimal (Decimal)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import GHC.Generics (Generic)
import Network.HTTP.Types qualified as HTTP
import Servant (Handler, ServerError (..), throwError)
import Text.Read (readMaybe)

import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Types qualified as Hledger

import Hedger.Application.AppM (AppM, Env, runAppM)
import Hedger.Application.Common (SnapshotFreshness (..), SnapshotMetadata (..))
import Hedger.Application.Error (AppError (..))
import Hedger.Domain.Account qualified as Account
import Hedger.Domain.InvestmentPerformance qualified as Performance
import Hedger.Domain.Period (Granularity (..))
import Hedger.Interface.HTTP.Core.Errors (appErrorStatus, toEnvelope)

runAction :: Env -> Maybe Text -> AppM value -> Handler value
runAction env requestId action = do
    result <- liftIO (runAppM env action)
    either (throwError . servantError requestId) pure result

servantError :: Maybe Text -> AppError -> ServerError
servantError requestId appError =
    ServerError
        { errHTTPCode = HTTP.statusCode (appErrorStatus appError)
        , errReasonPhrase = "Hedger API error"
        , errBody = Aeson.encode (toEnvelope (effectiveRequestId requestId) appError)
        , errHeaders = [(HTTP.hContentType, "application/json; charset=utf-8")]
        }

effectiveRequestId :: Maybe Text -> Text
effectiveRequestId = maybe "request-id-missing" id

defaultPageLimit :: Int
defaultPageLimit = 50

maximumPageLimit :: Int
maximumPageLimit = 200

parseLimit :: Maybe Text -> Either AppError Int
parseLimit Nothing = Right defaultPageLimit
parseLimit (Just raw) =
    case readMaybe (Text.unpack raw) of
        Just value | value >= 1 && value <= maximumPageLimit -> Right value
        _ -> Left (InvalidLimit defaultPageLimit maximumPageLimit)

parseDay :: Maybe Text -> Either AppError (Maybe Day)
parseDay Nothing = Right Nothing
parseDay (Just raw) =
    case parseTimeM True defaultTimeLocale "%F" (Text.unpack raw) of
        Just day | Text.pack (show day) == raw -> Right (Just day)
        _ -> Left InvalidDate

parseCanonicalAccountPath :: Text -> Either AppError Text
parseCanonicalAccountPath raw =
    let path = Text.strip raw
     in if Text.null path
            then Left (InvalidRequest "An account path is required.")
            else Right path

validateDateRange :: Maybe Day -> Maybe Day -> Either AppError ()
validateDateRange (Just start) (Just end)
    | start > end = Left InvalidDateRange
validateDateRange _ _ = Right ()

data ResponseMetadata = ResponseMetadata
    { revision :: Text
    , freshness :: Text
    , warnings :: [Text]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data CommodityQuantity = CommodityQuantity
    { commodity :: Text
    , quantity :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

commodityQuantities :: Hledger.MixedAmount -> [CommodityQuantity]
commodityQuantities =
    fmap toCommodityQuantity
        . HledgerAmount.amountsRaw
        . HledgerAmount.mixedAmountStripCosts

decimalText :: Decimal -> Text
decimalText = Text.pack . show

parseGranularity :: Text -> Either AppError Granularity
parseGranularity value = case Text.toLower value of
    "monthly" -> Right Monthly
    "yearly" -> Right Yearly
    _ -> Left (InvalidRequest "Granularity must be monthly or yearly.")

granularityText :: Granularity -> Text
granularityText Monthly = "monthly"
granularityText Yearly = "yearly"

valuationCompletenessText :: Account.ValuationCompleteness -> Text
valuationCompletenessText status = case status of
    Account.Complete -> "complete"
    Account.Partial -> "partial"
    Account.Unavailable -> "unavailable"

investmentCompletenessText :: Performance.InvestmentCompleteness -> Text
investmentCompletenessText status = case status of
    Performance.Complete -> "complete"
    Performance.Partial -> "partial"
    Performance.Unavailable -> "unavailable"

toCommodityQuantity :: Hledger.Amount -> CommodityQuantity
toCommodityQuantity amount =
    CommodityQuantity
        { commodity = amount.acommodity
        , quantity = Text.pack (show amount.aquantity)
        }

toResponseMetadata :: SnapshotMetadata -> ResponseMetadata
toResponseMetadata value =
    ResponseMetadata
        { revision = value.revision
        , freshness = freshnessText value.freshness
        , warnings = freshnessWarnings value.freshness
        }

freshnessText :: SnapshotFreshness -> Text
freshnessText Fresh = "fresh"
freshnessText Stale = "stale"

freshnessWarnings :: SnapshotFreshness -> [Text]
freshnessWarnings Fresh = []
freshnessWarnings Stale =
    ["The latest journal reload failed; the last usable snapshot is shown."]

accountTypeText :: Hledger.AccountType -> Text
accountTypeText accountType = case accountType of
    Hledger.Asset -> "asset"
    Hledger.Liability -> "liability"
    Hledger.Equity -> "equity"
    Hledger.Revenue -> "income"
    Hledger.Expense -> "expense"
    Hledger.Cash -> "cash"
    Hledger.Conversion -> "conversion"
    Hledger.Gain -> "gain"
