module Hedger.Application.Analytics (
    -- * Requests and read-models
    Granularity (..),
    AnalyticsRequest (..),
    ActivityKind (..),
    ActivityRange (..),
    ActivityRequest (..),
    Kpi (..),
    TimelinePoint (..),
    Magnitude,
    BreakdownItem (..),
    CategoryBreakdown (..),
    PerformancePoint (..),
    ConversionWarning (..),
    NetWorthReport (..),
    CashflowReport (..),
    ActivityBucket (..),
    ActivityEvent (..),
    AssetMovement (..),
    LiabilityMovement (..),
    FlowChange (..),
    ActivityIssue (..),
    ActivityDirection (..),
    DirectionBreakdown (..),
    ActivityReport (..),

    -- * Use cases
    netWorth,
    cashflow,
    activity,
) where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Decimal (Decimal)
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)

import Hledger.Data.Types qualified as Hledger
import Hledger.Query qualified as HledgerQuery

import Hedger.Application.AppM (AppM, Env (..), SnapshotContext (..), requireSnapshot)
import Hedger.Application.Common (
    AccountTypeClassifier,
    SnapshotMetadata,
    accountTypeClassifier,
    metadataFor,
 )
import Hedger.Application.TransactionQuery (parseHledgerQuery)
import Hedger.Config (Config (..), monthOfYearValue)
import Hedger.Domain.Account (DisplayDepth (..))
import Hedger.Domain.Activity (
    ActivityBuckets (..),
    ActivityDirection (..),
    ActivityEvent (..),
    ActivityFigures (..),
    ActivityIssue (..),
    ActivityKind (..),
    ActivityRange (..),
    AssetBucketFigures (..),
    AssetMovement (..),
    ExpenseBucketFigures (..),
    FlowChange (..),
    IncomeBucketFigures (..),
    LiabilityBucketFigures (..),
    LiabilityMovement (..),
    computeActivity,
    parseActivityPeriod,
 )
import Hedger.Domain.Cashflow (CashflowFigures (..), Magnitude, computeCashflow)
import Hedger.Domain.NetWorth (
    BreakdownItem (..),
    NetWorthFigures (..),
    PerformancePoint (..),
    TimelinePoint (..),
    computeNetWorth,
 )
import Hedger.Domain.Period (Granularity (..), atIndex, epoch, reportPeriodKey)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (ConversionWarning (..))

-- * Requests (unresolved boundary inputs) and UI read-models
data AnalyticsRequest = AnalyticsRequest
    { query :: Maybe Text
    , granularity :: Granularity
    , displayCommodity :: Maybe Text
    , positivePeriod :: Maybe Text
    , negativePeriod :: Maybe Text
    , positiveDepth :: DisplayDepth
    , negativeDepth :: DisplayDepth
    }

data ActivityRequest = ActivityRequest
    { kind :: ActivityKind
    , range :: ActivityRange
    , period :: Text
    , query :: Maybe Text
    , displayCommodity :: Maybe Text
    , depth :: DisplayDepth
    }

data Kpi = Kpi
    { value :: Decimal
    , absoluteChange :: Maybe Decimal
    , percentageChange :: Maybe Decimal
    , trend :: [Decimal]
    }

data CategoryBreakdown = CategoryBreakdown
    { period :: Text
    , total :: Decimal
    , items :: [BreakdownItem]
    , otherItems :: [BreakdownItem]
    }

data NetWorthReport = NetWorthReport
    { metadata :: SnapshotMetadata
    , displayCommodity :: Text
    , granularity :: Granularity
    , netWorthKpi :: Kpi
    , assetsKpi :: Kpi
    , liabilitiesKpi :: Kpi
    , timeline :: [TimelinePoint Decimal]
    , assetsBreakdown :: CategoryBreakdown
    , liabilitiesBreakdown :: CategoryBreakdown
    , performance :: [PerformancePoint]
    , incompletePeriods :: Int
    , warnings :: [ConversionWarning]
    , classifyAccount :: AccountTypeClassifier
    }

data CashflowReport = CashflowReport
    { metadata :: SnapshotMetadata
    , displayCommodity :: Text
    , granularity :: Granularity
    , totalCount :: Int
    , filteredCount :: Int
    , timelineAvailable :: Bool
    , timeline :: [TimelinePoint Magnitude]
    , incomeBreakdown :: CategoryBreakdown
    , expenseBreakdown :: CategoryBreakdown
    , warnings :: [ConversionWarning]
    , classifyAccount :: AccountTypeClassifier
    }

data ActivityBucket = ActivityBucket
    { key :: Text
    , startDate :: Day
    , total :: Decimal
    , increase :: Decimal
    , decrease :: Decimal
    , reallocated :: Decimal
    , categories :: [BreakdownItem]
    , directions :: [DirectionBreakdown]
    , posted :: Decimal
    , reversed :: Decimal
    , forgiven :: Decimal
    , borrowingReversed :: Decimal
    , events :: [ActivityEvent]
    , transactions :: [Hledger.Transaction]
    }

data DirectionBreakdown = DirectionBreakdown
    { direction :: ActivityDirection
    , categories :: [BreakdownItem]
    }

data ActivityReport = ActivityReport
    { metadata :: SnapshotMetadata
    , displayCommodity :: Text
    , kind :: ActivityKind
    , range :: ActivityRange
    , period :: Text
    , fiscalYearStartMonth :: Int
    , buckets :: [ActivityBucket]
    , warnings :: [ConversionWarning]
    , issues :: [ActivityIssue]
    , classifyAccount :: AccountTypeClassifier
    }

-- * Use cases: resolve inputs, call the domain, project figures into read-models.
netWorth :: AnalyticsRequest -> AppM NetWorthReport
netWorth request = do
    snapshotContext <- requireSnapshot
    commodity <- resolvedCommodity request.displayCommodity
    let snapshot = snapshotContext.snapshot
        responseMetadata = metadataFor snapshotContext
    matched <- parseTransactions snapshot request.query
    let figures =
            computeNetWorth
                snapshot
                matched
                request.granularity
                commodity
                request.positivePeriod
                request.negativePeriod
                request.positiveDepth
                request.negativeDepth
        points = figures.timeline
        selectedKey = figures.selectedKey
    pure
        NetWorthReport
            { metadata = responseMetadata
            , displayCommodity = commodity
            , granularity = request.granularity
            , netWorthKpi = useCurrent (kpiFor selectedKey (.net) points) figures.currentNetWorth
            , assetsKpi = useCurrent (kpiFor selectedKey (.positive) points) figures.currentAssets
            , liabilitiesKpi =
                useCurrent (kpiFor selectedKey (.negative) points) figures.currentLiabilities
            , timeline = points
            , assetsBreakdown =
                categoryBreakdown figures.assetsBreakdownPeriod figures.assetsBreakdownItems
            , liabilitiesBreakdown =
                categoryBreakdown
                    figures.liabilitiesBreakdownPeriod
                    figures.liabilitiesBreakdownItems
            , performance = figures.performance
            , incompletePeriods = figures.incompletePeriods
            , warnings = conversionWarnings figures.missing
            , classifyAccount = accountTypeClassifier snapshot.journal
            }

cashflow :: AnalyticsRequest -> AppM CashflowReport
cashflow request = do
    snapshotContext <- requireSnapshot
    commodity <- resolvedCommodity request.displayCommodity
    let snapshot = snapshotContext.snapshot
        responseMetadata = metadataFor snapshotContext
    matched <- parseTransactions snapshot request.query
    let figures =
            computeCashflow
                snapshot
                matched
                request.granularity
                commodity
                request.positivePeriod
                request.negativePeriod
                request.positiveDepth
                request.negativeDepth
    pure
        CashflowReport
            { metadata = responseMetadata
            , displayCommodity = commodity
            , granularity = request.granularity
            , totalCount = figures.totalCount
            , filteredCount = figures.filteredCount
            , timelineAvailable = figures.available
            , timeline = figures.timeline
            , incomeBreakdown =
                categoryBreakdown figures.incomeBreakdownPeriod figures.incomeBreakdownItems
            , expenseBreakdown =
                categoryBreakdown figures.expenseBreakdownPeriod figures.expenseBreakdownItems
            , warnings = conversionWarnings figures.missing
            , classifyAccount = accountTypeClassifier snapshot.journal
            }

activity :: ActivityRequest -> AppM ActivityReport
activity request = do
    snapshotContext <- requireSnapshot
    commodity <- resolvedCommodity request.displayCommodity
    fiscalStart <- asks (.config.fiscalYearStartMonth)
    let snapshot = snapshotContext.snapshot
        responseMetadata = metadataFor snapshotContext
    filtered <- parseTransactions snapshot request.query
    activityPeriod <-
        either
            throwError
            pure
            (parseActivityPeriod (monthOfYearValue fiscalStart) request.range request.period)
    let figures =
            computeActivity
                snapshot
                filtered
                commodity
                request.kind
                request.range
                request.depth
                activityPeriod
    pure
        ActivityReport
            { metadata = responseMetadata
            , displayCommodity = commodity
            , kind = request.kind
            , range = request.range
            , period = reportPeriodKey activityPeriod
            , fiscalYearStartMonth = monthOfYearValue fiscalStart
            , buckets = toActivityBuckets figures.buckets
            , warnings = conversionWarnings figures.missing
            , issues = Set.toAscList figures.issues
            , classifyAccount = accountTypeClassifier snapshot.journal
            }

-- * Input resolution
resolvedCommodity :: Maybe Text -> AppM Text
resolvedCommodity requested = do
    configured <- asks (.config.defaultDisplayCommodity)
    pure (fromMaybe configured (requested >>= nonEmpty))

nonEmpty :: Text -> Maybe Text
nonEmpty value
    | Text.null (Text.strip value) = Nothing
    | otherwise = Just (Text.strip value)

parseTransactions :: Snapshot -> Maybe Text -> AppM [Hledger.Transaction]
parseTransactions snapshot requested = do
    let canonical = Text.strip (fromMaybe "" requested)
        reference = fromMaybe epoch snapshot.latestJournalDate
    parsed <- either throwError pure (parseHledgerQuery reference canonical)
    pure (filter (HledgerQuery.matchesTransaction parsed) snapshot.transactions)

-- * Read-model shaping (presentation, not calculation)
kpiFor :: Maybe Text -> (TimelinePoint Decimal -> Decimal) -> [TimelinePoint Decimal] -> Kpi
kpiFor selectedKey select points =
    let selectedIndex =
            fromMaybe
                (max 0 (length points - 1))
                (selectedKey >>= (\key -> List.findIndex ((== key) . (.period)) points))
        selected = fromMaybe zeroTimeline (atIndex selectedIndex points)
        previous = atIndex (selectedIndex - 1) points
        currentValue = select selected
        change = fmap (\prior -> currentValue - select prior) previous
        percentage = previous >>= (percentageFrom currentValue . select)
        trendStart = max 0 (selectedIndex - 11)
        trendValues = fmap select (take (selectedIndex - trendStart + 1) (drop trendStart points))
     in Kpi
            { value = currentValue
            , absoluteChange = change
            , percentageChange = percentage
            , trend = trendValues
            }

useCurrent :: Kpi -> Maybe Decimal -> Kpi
useCurrent fallback current =
    case current of
        Nothing -> fallback
        Just amount ->
            Kpi
                { value = amount
                , absoluteChange = fallback.absoluteChange
                , percentageChange = fallback.percentageChange
                , trend = fallback.trend
                }

percentageFrom :: Decimal -> Decimal -> Maybe Decimal
percentageFrom current previous
    | abs previous < 0.000001 = Nothing
    | otherwise = Just (((current - previous) / abs previous) * 100)

zeroTimeline :: TimelinePoint Decimal
zeroTimeline =
    TimelinePoint
        { period = ""
        , startDate = epoch
        , positive = 0
        , negative = 0
        , net = 0
        , missing = Set.empty
        }

categoryBreakdown :: Text -> [BreakdownItem] -> CategoryBreakdown
categoryBreakdown period items =
    let (grouped, otherItems) = groupOther items
     in CategoryBreakdown
            { period
            , total = sum (fmap (.quantity) grouped)
            , items = grouped
            , otherItems
            }

groupOther :: [BreakdownItem] -> ([BreakdownItem], [BreakdownItem])
groupOther items =
    let sorted = List.sortOn (\item -> (negate item.quantity, item.account)) items
        total = sum (fmap (.quantity) sorted)
        visible item = total > 0 && item.quantity / total >= 0.01
        candidates = filter visible sorted
        named =
            take
                ( if length candidates > 10
                    then 9
                    else 10
                )
                candidates
        namedAccounts = Set.fromList (fmap (.account) named)
        others = [item | item <- sorted, not (Set.member item.account namedAccounts)]
        other = sum (fmap (.quantity) others)
     in if other > 0
            then (named <> [BreakdownItem{account = "Other", quantity = other}], others)
            else (named, [])

toActivityBuckets :: ActivityBuckets -> [ActivityBucket]
toActivityBuckets typed =
    case typed of
        AssetBuckets buckets -> fmap assetBucket buckets
        LiabilityBuckets buckets -> fmap liabilityBucket buckets
        IncomeBuckets buckets -> fmap incomeBucket buckets
        ExpenseBuckets buckets -> fmap expenseBucket buckets

assetBucket :: AssetBucketFigures -> ActivityBucket
assetBucket figures =
    ActivityBucket
        { key = figures.key
        , startDate = figures.startDate
        , total = figures.acquired + figures.disposed + figures.transferred
        , increase = figures.acquired
        , decrease = figures.disposed
        , reallocated = figures.transferred
        , categories = fst (groupOther figures.categoryItems)
        , directions = directionBreakdowns figures.directionItems
        , posted = 0
        , reversed = 0
        , forgiven = 0
        , borrowingReversed = 0
        , events = fmap AssetEvent figures.movements
        , transactions = figures.transactions
        }

liabilityBucket :: LiabilityBucketFigures -> ActivityBucket
liabilityBucket figures =
    ActivityBucket
        { key = figures.key
        , startDate = figures.startDate
        , total = figures.borrowed + figures.repaid + figures.forgiven + figures.borrowingReversed
        , increase = figures.borrowed
        , decrease = figures.repaid + figures.forgiven + figures.borrowingReversed
        , reallocated = 0
        , categories = fst (groupOther figures.categoryItems)
        , directions = directionBreakdowns figures.directionItems
        , posted = 0
        , reversed = 0
        , forgiven = figures.forgiven
        , borrowingReversed = figures.borrowingReversed
        , events = fmap LiabilityEvent figures.movements
        , transactions = figures.transactions
        }

incomeBucket :: IncomeBucketFigures -> ActivityBucket
incomeBucket figures =
    ActivityBucket
        { key = figures.key
        , startDate = figures.startDate
        , total = figures.posted - figures.reversed
        , increase = 0
        , decrease = 0
        , reallocated = 0
        , categories = fst (groupOther figures.categoryItems)
        , directions = []
        , posted = figures.posted
        , reversed = figures.reversed
        , forgiven = 0
        , borrowingReversed = 0
        , events = [IncomeEvent account change | (account, change) <- figures.changes]
        , transactions = figures.transactions
        }

expenseBucket :: ExpenseBucketFigures -> ActivityBucket
expenseBucket figures =
    ActivityBucket
        { key = figures.key
        , startDate = figures.startDate
        , total = figures.posted - figures.reversed
        , increase = 0
        , decrease = 0
        , reallocated = 0
        , categories = fst (groupOther figures.categoryItems)
        , directions = []
        , posted = figures.posted
        , reversed = figures.reversed
        , forgiven = 0
        , borrowingReversed = 0
        , events = [ExpenseEvent account change | (account, change) <- figures.changes]
        , transactions = figures.transactions
        }

directionBreakdowns :: [(ActivityDirection, [BreakdownItem])] -> [DirectionBreakdown]
directionBreakdowns items =
    [ DirectionBreakdown{direction, categories = fst (groupOther categories)}
    | (direction, categories) <- items
    ]

conversionWarnings :: Set Text -> [ConversionWarning]
conversionWarnings missing
    | Set.null missing = []
    | otherwise = [MissingConversion missing]
