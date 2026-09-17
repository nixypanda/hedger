module Hedger.Interface.HTTP.Endpoints.Analytics (
    AnalyticsAPI,
    analyticsServer,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)
import GHC.Generics (Generic)
import Servant

import Hedger.Application.Analytics qualified as Analytics
import Hedger.Application.AppM (Env)
import Hedger.Application.Common (AccountTypeClassifier)
import Hedger.Domain.Error (AppError (..))
import Hedger.Interface.HTTP.Core.Common (
    ResponseMetadata,
    accountTypeText,
    decimalText,
    granularityText,
    parseGranularity,
    runAction,
    servantError,
    toResponseMetadata,
 )
import Hedger.Interface.HTTP.Endpoints.Transactions (TransactionDto, toTransactionDto)

-- DTO declaration
data AnalyticsRequestDto = AnalyticsRequestDto
    { query :: Maybe Text
    , granularity :: Text
    , displayCommodity :: Maybe Text
    , positivePeriod :: Maybe Text
    , negativePeriod :: Maybe Text
    , positiveDepth :: Maybe Int
    , negativeDepth :: Maybe Int
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ActivityRequestDto = ActivityRequestDto
    { kind :: Text
    , range :: Text
    , period :: Text
    , query :: Maybe Text
    , displayCommodity :: Maybe Text
    , depth :: Maybe Int
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data KpiDto = KpiDto
    { value :: Text
    , absoluteChange :: Maybe Text
    , percentageChange :: Maybe Text
    , trend :: [Text]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

data TimelinePointDto = TimelinePointDto
    { period :: Text
    , startDate :: Day
    , positive :: Text
    , negative :: Text
    , net :: Text
    , completeness :: Text
    , missingCommodities :: [Text]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

data BreakdownItemDto = BreakdownItemDto
    { account :: Text
    , accountType :: Maybe Text
    , quantity :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

data CategoryBreakdownDto = CategoryBreakdownDto
    { period :: Text
    , total :: Text
    , items :: [BreakdownItemDto]
    , otherItems :: [BreakdownItemDto]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

data PerformancePointDto = PerformancePointDto
    { period :: Text
    , startDate :: Day
    , netInvestment :: Text
    , netWorth :: Text
    , gainLoss :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

data NetWorthResponse = NetWorthResponse
    { metadata :: ResponseMetadata
    , displayCommodity :: Text
    , granularity :: Text
    , netWorth :: KpiDto
    , assets :: KpiDto
    , liabilities :: KpiDto
    , timeline :: [TimelinePointDto]
    , assetsBreakdown :: CategoryBreakdownDto
    , liabilitiesBreakdown :: CategoryBreakdownDto
    , performance :: [PerformancePointDto]
    , incompletePeriods :: Int
    , warnings :: [Text]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

data CashflowResponse = CashflowResponse
    { metadata :: ResponseMetadata
    , displayCommodity :: Text
    , granularity :: Text
    , totalCount :: Int
    , filteredCount :: Int
    , timelineAvailable :: Bool
    , timeline :: [TimelinePointDto]
    , incomeBreakdown :: CategoryBreakdownDto
    , expenseBreakdown :: CategoryBreakdownDto
    , warnings :: [Text]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

data ActivityBucketDto = ActivityBucketDto
    { key :: Text
    , startDate :: Day
    , total :: Text
    , increase :: Text
    , decrease :: Text
    , reallocated :: Text
    , categories :: [BreakdownItemDto]
    , directions :: [DirectionBreakdownDto]
    , transactions :: [TransactionDto]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

data DirectionBreakdownDto = DirectionBreakdownDto
    { direction :: Text
    , categories :: [BreakdownItemDto]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

data ActivityResponse = ActivityResponse
    { metadata :: ResponseMetadata
    , displayCommodity :: Text
    , kind :: Text
    , range :: Text
    , period :: Text
    , fiscalYearStartMonth :: Int
    , buckets :: [ActivityBucketDto]
    , warnings :: [Text]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

-- API and server
type AnalyticsAPI =
    "net-worth" :> "query" :> ReqBody '[JSON] AnalyticsRequestDto :> Post '[JSON] NetWorthResponse
        :<|> "cashflow"
            :> "query"
            :> ReqBody
                '[JSON]
                AnalyticsRequestDto
            :> Post '[JSON] CashflowResponse
        :<|> "activity"
            :> "query"
            :> ReqBody
                '[JSON]
                ActivityRequestDto
            :> Post '[JSON] ActivityResponse

analyticsServer :: Env -> Maybe Text -> Server AnalyticsAPI
analyticsServer env requestId = handleNetWorth :<|> handleCashflow :<|> handleActivity
  where
    handleNetWorth request =
        case toAnalyticsRequest request of
            Left err -> throwError (servantError requestId err)
            Right parsed -> runAction env requestId (toNetWorth <$> Analytics.netWorth parsed)
    handleCashflow request =
        case toAnalyticsRequest request of
            Left err -> throwError (servantError requestId err)
            Right parsed -> runAction env requestId (toCashflow <$> Analytics.cashflow parsed)
    handleActivity request =
        case toActivityRequest request of
            Left err -> throwError (servantError requestId err)
            Right parsed -> runAction env requestId (toActivity <$> Analytics.activity parsed)

-- Mapper: domain <-> DTO
toAnalyticsRequest :: AnalyticsRequestDto -> Either AppError Analytics.AnalyticsRequest
toAnalyticsRequest request = do
    granularity <- parseGranularity request.granularity
    positiveDepth <- parseDepth request.positiveDepth
    negativeDepth <- parseDepth request.negativeDepth
    pure
        Analytics.AnalyticsRequest
            { query = request.query
            , granularity
            , displayCommodity = request.displayCommodity
            , positivePeriod = request.positivePeriod
            , negativePeriod = request.negativePeriod
            , positiveDepth
            , negativeDepth
            }

toActivityRequest :: ActivityRequestDto -> Either AppError Analytics.ActivityRequest
toActivityRequest request = do
    kind <- parseActivityKind request.kind
    range <- parseActivityRange request.range
    depth <- parseDepth request.depth
    pure
        Analytics.ActivityRequest
            { kind
            , range
            , period = request.period
            , query = request.query
            , displayCommodity = request.displayCommodity
            , depth
            }

parseActivityKind :: Text -> Either AppError Analytics.ActivityKind
parseActivityKind value =
    case Text.toLower value of
        "income" -> Right Analytics.IncomeActivity
        "expense" -> Right Analytics.ExpenseActivity
        "asset" -> Right Analytics.AssetActivity
        "liability" -> Right Analytics.LiabilityActivity
        _ -> Left (InvalidRequest "Activity kind must be income, expense, asset, or liability.")

parseActivityRange :: Text -> Either AppError Analytics.ActivityRange
parseActivityRange value =
    case Text.toLower value of
        "month" -> Right Analytics.MonthActivity
        "year" -> Right Analytics.FiscalYearActivity
        _ -> Left (InvalidRequest "Activity range must be month or year.")

parseDepth :: Maybe Int -> Either AppError Int
parseDepth Nothing = Right 2
parseDepth (Just value)
    | value >= 1 && value <= 4 = Right value
    | otherwise = Left (InvalidRequest "Category depth must be between 1 and 4.")

toNetWorth :: Analytics.NetWorthReport -> NetWorthResponse
toNetWorth report =
    NetWorthResponse
        { metadata = toResponseMetadata report.metadata
        , displayCommodity = report.displayCommodity
        , granularity = granularityText report.granularity
        , netWorth = toKpi report.netWorthKpi
        , assets = toKpi report.assetsKpi
        , liabilities = toKpi report.liabilitiesKpi
        , timeline = fmap toTimeline report.timeline
        , assetsBreakdown = toBreakdown report.classifyAccount report.assetsBreakdown
        , liabilitiesBreakdown = toBreakdown report.classifyAccount report.liabilitiesBreakdown
        , performance = fmap toPerformance report.performance
        , incompletePeriods = report.incompletePeriods
        , warnings = fmap renderWarning report.warnings
        }

toCashflow :: Analytics.CashflowReport -> CashflowResponse
toCashflow report =
    CashflowResponse
        { metadata = toResponseMetadata report.metadata
        , displayCommodity = report.displayCommodity
        , granularity = granularityText report.granularity
        , totalCount = report.totalCount
        , filteredCount = report.filteredCount
        , timelineAvailable = report.timelineAvailable
        , timeline = fmap toTimeline report.timeline
        , incomeBreakdown = toBreakdown report.classifyAccount report.incomeBreakdown
        , expenseBreakdown = toBreakdown report.classifyAccount report.expenseBreakdown
        , warnings = fmap renderWarning report.warnings
        }

toActivity :: Analytics.ActivityReport -> ActivityResponse
toActivity report =
    ActivityResponse
        { metadata = toResponseMetadata report.metadata
        , displayCommodity = report.displayCommodity
        , kind = activityKindText report.kind
        , range = activityRangeText report.range
        , period = report.period
        , fiscalYearStartMonth = report.fiscalYearStartMonth
        , buckets = fmap (toActivityBucket report.classifyAccount) report.buckets
        , warnings = fmap renderWarning report.warnings
        }

toKpi :: Analytics.Kpi -> KpiDto
toKpi value =
    KpiDto
        { value = decimalText value.value
        , absoluteChange = decimalText <$> value.absoluteChange
        , percentageChange = decimalText <$> value.percentageChange
        , trend = decimalText <$> value.trend
        }

toTimeline :: Analytics.TimelinePoint -> TimelinePointDto
toTimeline value =
    TimelinePointDto
        { period = value.period
        , startDate = value.startDate
        , positive = decimalText value.positive
        , negative = decimalText value.negative
        , net = decimalText value.net
        , completeness =
            if Set.null value.missing
                then "complete"
                else "partial"
        , missingCommodities = Set.toAscList value.missing
        }

toBreakdown :: AccountTypeClassifier -> Analytics.CategoryBreakdown -> CategoryBreakdownDto
toBreakdown classify value =
    CategoryBreakdownDto
        { period = value.period
        , total = decimalText value.total
        , items = fmap (toBreakdownItem classify) value.items
        , otherItems = fmap (toBreakdownItem classify) value.otherItems
        }

toBreakdownItem :: AccountTypeClassifier -> Analytics.BreakdownItem -> BreakdownItemDto
toBreakdownItem classify value =
    BreakdownItemDto
        { account = value.account
        , accountType = accountTypeText <$> classify value.account
        , quantity = decimalText value.quantity
        }

toPerformance :: Analytics.PerformancePoint -> PerformancePointDto
toPerformance value =
    PerformancePointDto
        { period = value.period
        , startDate = value.startDate
        , netInvestment = decimalText value.netInvestment
        , netWorth = decimalText value.netWorth
        , gainLoss = decimalText value.gainLoss
        }

toActivityBucket :: AccountTypeClassifier -> Analytics.ActivityBucket -> ActivityBucketDto
toActivityBucket classify value =
    ActivityBucketDto
        { key = value.key
        , startDate = value.startDate
        , total = decimalText value.total
        , increase = decimalText value.increase
        , decrease = decimalText value.decrease
        , reallocated = decimalText value.reallocated
        , categories = fmap (toBreakdownItem classify) value.categories
        , directions = fmap (toDirectionBreakdown classify) value.directions
        , transactions = fmap (toTransactionDto classify) value.transactions
        }

toDirectionBreakdown ::
    AccountTypeClassifier -> Analytics.DirectionBreakdown -> DirectionBreakdownDto
toDirectionBreakdown classify value =
    DirectionBreakdownDto
        { direction = directionText value.direction
        , categories = fmap (toBreakdownItem classify) value.categories
        }

directionText :: Analytics.ActivityDirection -> Text
directionText direction =
    case direction of
        Analytics.Increase -> "increase"
        Analytics.Reallocate -> "reallocate"
        Analytics.Decrease -> "decrease"

activityKindText :: Analytics.ActivityKind -> Text
activityKindText kind =
    case kind of
        Analytics.IncomeActivity -> "income"
        Analytics.ExpenseActivity -> "expense"
        Analytics.AssetActivity -> "asset"
        Analytics.LiabilityActivity -> "liability"

activityRangeText :: Analytics.ActivityRange -> Text
activityRangeText range =
    case range of
        Analytics.MonthActivity -> "month"
        Analytics.FiscalYearActivity -> "year"

renderWarning :: Analytics.ConversionWarning -> Text
renderWarning (Analytics.MissingConversion missing) =
    "Missing dated conversion to the display commodity for: "
        <> Text.intercalate ", " (Set.toAscList missing)
