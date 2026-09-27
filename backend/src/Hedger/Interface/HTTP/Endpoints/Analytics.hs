module Hedger.Interface.HTTP.Endpoints.Analytics (
    AnalyticsAPI,
    analyticsServer,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Decimal (Decimal)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)
import GHC.Generics (Generic)
import Servant

import Hedger.Application.Analytics qualified as Analytics
import Hedger.Application.AppM (Env)
import Hedger.Application.Common (AccountTypeClassifier)
import Hedger.Domain.Account (DisplayDepth (..), parseDisplayDepth)
import Hedger.Domain.ActivityEvent (
    assetAccountName,
    expenseAccountName,
    incomeAccountName,
    liabilityAccountName,
    positiveAmountValue,
 )
import Hedger.Domain.Cashflow (magnitudeValue)
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
    , posted :: Text
    , reversed :: Text
    , borrowed :: Text
    , repaid :: Text
    , forgiven :: Text
    , borrowingReversed :: Text
    , netDebtMovement :: Text
    , categories :: [BreakdownItemDto]
    , directions :: [DirectionBreakdownDto]
    , events :: [ActivityEventDto]
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

data ActivityEventDto = ActivityEventDto
    { effect :: Text
    , account :: Maybe Text
    , source :: Maybe Text
    , destination :: Maybe Text
    , amount :: Maybe Text
    , sourceAmount :: Maybe Text
    , destinationAmount :: Maybe Text
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

parseDepth :: Maybe Int -> Either AppError DisplayDepth
parseDepth Nothing = Right Depth2
parseDepth (Just value) =
    case parseDisplayDepth value of
        Just depth -> Right depth
        Nothing -> Left (InvalidRequest "Category depth must be between 1 and 4.")

toNetWorth :: Analytics.NetWorthReport -> NetWorthResponse
toNetWorth report =
    NetWorthResponse
        { metadata = toResponseMetadata report.metadata
        , displayCommodity = report.displayCommodity
        , granularity = granularityText report.granularity
        , netWorth = toKpi report.netWorthKpi
        , assets = toKpi report.assetsKpi
        , liabilities = toKpi report.liabilitiesKpi
        , timeline = fmap (toTimeline decimalText) report.timeline
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
        , timeline = fmap (toTimeline (decimalText . magnitudeValue)) report.timeline
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
        , buckets = fmap (toActivityBucket report.kind report.classifyAccount) report.buckets
        , warnings = fmap renderWarning report.warnings <> fmap renderActivityIssue report.issues
        }

toKpi :: Analytics.Kpi -> KpiDto
toKpi value =
    KpiDto
        { value = decimalText value.value
        , absoluteChange = decimalText <$> value.absoluteChange
        , percentageChange = decimalText <$> value.percentageChange
        , trend = decimalText <$> value.trend
        }

toTimeline :: (side -> Text) -> Analytics.TimelinePoint side -> TimelinePointDto
toTimeline render value =
    TimelinePointDto
        { period = value.period
        , startDate = value.startDate
        , positive = render value.positive
        , negative = render value.negative
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

toActivityBucket ::
    Analytics.ActivityKind ->
    AccountTypeClassifier ->
    Analytics.ActivityBucket ->
    ActivityBucketDto
toActivityBucket kind classify value =
    ActivityBucketDto
        { key = value.key
        , startDate = value.startDate
        , total = decimalText value.total
        , increase = decimalText value.increase
        , decrease = decimalText value.decrease
        , reallocated = decimalText value.reallocated
        , posted = decimalText value.posted
        , reversed = decimalText value.reversed
        , borrowed = debtAmount value.increase
        , repaid = debtAmount (value.decrease - value.forgiven - value.borrowingReversed)
        , forgiven = decimalText value.forgiven
        , borrowingReversed = decimalText value.borrowingReversed
        , netDebtMovement = debtAmount (value.increase - value.decrease)
        , categories = fmap (toBreakdownItem classify) value.categories
        , directions = fmap (toDirectionBreakdown classify) value.directions
        , events = fmap toActivityEvent value.events
        , transactions = fmap (toTransactionDto classify) value.transactions
        }
  where
    debtAmount amount =
        decimalText $ case kind of
            Analytics.LiabilityActivity -> amount
            _ -> 0

toActivityEvent :: Analytics.ActivityEvent -> ActivityEventDto
toActivityEvent event =
    case event of
        Analytics.AssetEvent movement ->
            case movement of
                Analytics.Acquired account quantity ->
                    simpleEvent "acquired" (assetAccountName account) (positiveAmountValue quantity)
                Analytics.Disposed account quantity ->
                    simpleEvent "disposed" (assetAccountName account) (positiveAmountValue quantity)
                Analytics.Transferred from outgoing to incoming ->
                    linkedEvent
                        "transferred"
                        (assetAccountName from)
                        (positiveAmountValue outgoing)
                        (assetAccountName to)
                        (positiveAmountValue incoming)
        Analytics.LiabilityEvent movement ->
            case movement of
                Analytics.Borrowed account quantity ->
                    simpleEvent
                        "borrowed"
                        (liabilityAccountName account)
                        (positiveAmountValue quantity)
                Analytics.Repaid account quantity ->
                    simpleEvent
                        "repaid"
                        (liabilityAccountName account)
                        (positiveAmountValue quantity)
                Analytics.Refinanced old repaid new borrowed ->
                    linkedEvent
                        "refinanced"
                        (liabilityAccountName old)
                        (positiveAmountValue repaid)
                        (liabilityAccountName new)
                        (positiveAmountValue borrowed)
                Analytics.Forgiven account quantity ->
                    simpleEvent
                        "forgiven"
                        (liabilityAccountName account)
                        (positiveAmountValue quantity)
                Analytics.BorrowingReversed account quantity ->
                    simpleEvent
                        "borrowing_reversed"
                        (liabilityAccountName account)
                        (positiveAmountValue quantity)
        Analytics.IncomeEvent account change ->
            flowEvent "income" (incomeAccountName account) change
        Analytics.ExpenseEvent account change ->
            flowEvent "expense" (expenseAccountName account) change

simpleEvent :: Text -> Text -> Decimal -> ActivityEventDto
simpleEvent effect account quantity =
    ActivityEventDto
        { effect
        , account = Just account
        , source = Nothing
        , destination = Nothing
        , amount = Just (decimalText quantity)
        , sourceAmount = Nothing
        , destinationAmount = Nothing
        }

linkedEvent :: Text -> Text -> Decimal -> Text -> Decimal -> ActivityEventDto
linkedEvent effect from outgoing to incoming =
    ActivityEventDto
        { effect
        , account = Nothing
        , source = Just from
        , destination = Just to
        , amount = Nothing
        , sourceAmount = Just (decimalText outgoing)
        , destinationAmount = Just (decimalText incoming)
        }

flowEvent :: Text -> Text -> Analytics.FlowChange -> ActivityEventDto
flowEvent kind account change =
    case change of
        Analytics.Posted quantity ->
            simpleEvent (kind <> "_posted") account (positiveAmountValue quantity)
        Analytics.Reversed quantity ->
            simpleEvent (kind <> "_reversed") account (positiveAmountValue quantity)

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
        <> Text.intercalate ", " (NonEmpty.toList missing)

renderActivityIssue :: Analytics.ActivityIssue -> Text
renderActivityIssue issue =
    case issue of
        Analytics.AmbiguousAssetMovement ->
            "Some asset movements could not be paired unambiguously."
        Analytics.AmbiguousLiabilityMovement ->
            "Some debt movements could not be paired unambiguously."
        Analytics.UnsupportedAssetMovement -> "Some asset movements could not be classified."
        Analytics.UnsupportedLiabilityMovement -> "Some debt reductions could not be classified."
