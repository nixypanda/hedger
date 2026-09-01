module Hedger.Application.Analytics (
    -- * Requests and read-models
    Granularity (..),
    AnalyticsRequest (..),
    ActivityKind (..),
    ActivityRange (..),
    ActivityRequest (..),
    Kpi (..),
    TimelinePoint (..),
    BreakdownItem (..),
    CategoryBreakdown (..),
    PerformancePoint (..),
    ConversionWarning (..),
    NetWorthReport (..),
    CashflowReport (..),
    ActivityBucket (..),
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
import Hedger.Application.Cache (contributeCheckpoints, usableCheckpoints)
import Hedger.Application.Common (
    AccountTypeClassifier,
    SnapshotMetadata,
    accountTypeClassifier,
    currentMetadata,
 )
import Hedger.Application.TransactionQuery (parseHledgerQuery)
import Hedger.Config (Config (..))
import Hedger.Domain.Activity (
    ActivityBucketFigures (..),
    ActivityDirection (..),
    ActivityFigures (..),
    ActivityKind (..),
    ActivityRange (..),
    computeActivity,
    parseActivityPeriod,
 )
import Hedger.Domain.Cashflow (CashflowFigures (..), computeCashflow, computeCashflowResumed)
import Hedger.Domain.NetWorth (
    BreakdownItem (..),
    NetWorthFigures (..),
    PerformancePoint (..),
    QueryKey (..),
    TimelinePoint (..),
    computeNetWorth,
    computeNetWorthResumed,
 )
import Hedger.Domain.Period (Granularity (..), ReportPeriod (..), atIndex, epoch)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (ConversionWarning (..))
import Hedger.Infrastructure.DerivedRevision qualified as DerivedRevision

-- * Requests (unresolved boundary inputs) and UI read-models

data AnalyticsRequest = AnalyticsRequest
    { query :: Maybe Text
    , granularity :: Granularity
    , displayCommodity :: Maybe Text
    , positivePeriod :: Maybe Text
    , negativePeriod :: Maybe Text
    , positiveDepth :: Int
    , negativeDepth :: Int
    }

data ActivityRequest = ActivityRequest
    { kind :: ActivityKind
    , range :: ActivityRange
    , period :: Text
    , query :: Maybe Text
    , displayCommodity :: Maybe Text
    , depth :: Int
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
    , timeline :: [TimelinePoint]
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
    , timeline :: [TimelinePoint]
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
    , classifyAccount :: AccountTypeClassifier
    }

-- * Use cases: resolve inputs, call the domain, project figures into read-models.

netWorth :: AnalyticsRequest -> AppM NetWorthReport
netWorth request = do
    snapshotContext <- requireSnapshot
    responseMetadata <- currentMetadata snapshotContext
    commodity <- resolvedCommodity request.displayCommodity
    let snapshot = snapshotContext.snapshot
    (queryKey, matched) <- parseTransactions snapshot request.query
    figures <- netWorthFigures snapshot queryKey matched request commodity
    let points = figures.timeline
        selectedKey = figures.selectedKey
    pure
        NetWorthReport
            { metadata = responseMetadata
            , displayCommodity = commodity
            , granularity = request.granularity
            , netWorthKpi = useCurrent (kpiFor selectedKey (.net) points) figures.currentNetWorth
            , assetsKpi = useCurrent (kpiFor selectedKey (.positive) points) figures.currentAssets
            , liabilitiesKpi = useCurrent (kpiFor selectedKey (.negative) points) figures.currentLiabilities
            , timeline = points
            , assetsBreakdown = categoryBreakdown figures.assetsBreakdownPeriod figures.assetsBreakdownItems
            , liabilitiesBreakdown =
                categoryBreakdown figures.liabilitiesBreakdownPeriod figures.liabilitiesBreakdownItems
            , performance = figures.performance
            , incompletePeriods = figures.incompletePeriods
            , warnings = conversionWarnings figures.missing
            , classifyAccount = accountTypeClassifier snapshot.journal
            }

cashflow :: AnalyticsRequest -> AppM CashflowReport
cashflow request = do
    snapshotContext <- requireSnapshot
    responseMetadata <- currentMetadata snapshotContext
    commodity <- resolvedCommodity request.displayCommodity
    let snapshot = snapshotContext.snapshot
    (queryKey, matched) <- parseTransactions snapshot request.query
    figures <- cashflowFigures snapshot queryKey matched request commodity
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
    responseMetadata <- currentMetadata snapshotContext
    commodity <- resolvedCommodity request.displayCommodity
    fiscalStart <- asks (.config.fiscalYearStartMonth)
    let snapshot = snapshotContext.snapshot
    filtered <- case request.kind of
        IncomeActivity -> snd <$> parseTransactions snapshot request.query
        ExpenseActivity -> snd <$> parseTransactions snapshot request.query
        AssetActivity -> pure snapshot.transactions
        LiabilityActivity -> pure snapshot.transactions
    activityPeriod <-
        either throwError pure (parseActivityPeriod fiscalStart request.range request.period)
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
            , period = activityPeriod.key
            , fiscalYearStartMonth = fiscalStart
            , buckets = fmap toActivityBucket figures.buckets
            , warnings = conversionWarnings figures.missing
            , classifyAccount = accountTypeClassifier snapshot.journal
            }

-- * Calculation, resumed from the derived view's checkpoints where it can be

{- | 'computeNetWorth', with the balance sheet's columns accumulated from
checkpointed opening balances when the derived view describes this very snapshot,
and computed from the start of the journal when it does not.
-}
netWorthFigures ::
    Snapshot ->
    QueryKey ->
    [Hledger.Transaction] ->
    AnalyticsRequest ->
    Text ->
    AppM NetWorthFigures
netWorthFigures snapshot queryKey matched request commodity = do
    checkpoints <- usableCheckpoints snapshot
    case checkpoints of
        Nothing ->
            pure
                ( computeNetWorth
                    snapshot
                    matched
                    request.granularity
                    commodity
                    request.positivePeriod
                    request.negativePeriod
                    request.positiveDepth
                    request.negativeDepth
                )
        Just (chain, caches) -> do
            let (figures, openings) =
                    computeNetWorthResumed
                        snapshot
                        matched
                        queryKey
                        chain
                        caches.netWorth
                        request.granularity
                        commodity
                        request.positivePeriod
                        request.negativePeriod
                        request.positiveDepth
                        request.negativeDepth
            contributeCheckpoints snapshot $ \current ->
                current{DerivedRevision.netWorth = forcedCache openings}
            pure figures

{- | 'computeCashflow', with each month's column taken from the checkpointed
columns when the derived view describes this very snapshot.
-}
cashflowFigures ::
    Snapshot ->
    QueryKey ->
    [Hledger.Transaction] ->
    AnalyticsRequest ->
    Text ->
    AppM CashflowFigures
cashflowFigures snapshot queryKey matched request commodity = do
    checkpoints <- usableCheckpoints snapshot
    case checkpoints of
        Nothing ->
            pure
                ( computeCashflow
                    snapshot
                    matched
                    request.granularity
                    commodity
                    request.positivePeriod
                    request.negativePeriod
                    request.positiveDepth
                    request.negativeDepth
                )
        Just (chain, caches) -> do
            let (figures, columns) =
                    computeCashflowResumed
                        snapshot
                        matched
                        chain
                        caches.cashflow
                        queryKey.text
                        request.granularity
                        commodity
                        request.positivePeriod
                        request.negativePeriod
                        request.positiveDepth
                        request.negativeDepth
            contributeCheckpoints snapshot $ \current ->
                current{DerivedRevision.cashflow = forcedCache columns}
            pure figures

{- | Force a checkpoint cache before it reaches the derived view.

Each stored balance and column is already forced as it is inserted, but the cache
itself arrives as a thunk, so publishing it unforced would leave this request's
accumulation — and the hledger reports behind it — retained and deferred into
whichever later request first reads a checkpoint. Each cache is a map, and the
sweeps that build one insert strictly, so forcing it to weak head normal form
runs the insert fold and with it every payload the fold stored.

Deliberately not left to the strict fields of
'Hedger.Infrastructure.DerivedRevision.DerivedCaches': the deferral this avoids
is a property of what the use case hands over, not of where it is stored.
-}
forcedCache :: cache -> cache
forcedCache cache = cache `seq` cache

-- * Input resolution

resolvedCommodity :: Maybe Text -> AppM Text
resolvedCommodity requested = do
    configured <- asks (.config.defaultDisplayCommodity)
    pure (fromMaybe configured (requested >>= nonEmpty))

nonEmpty :: Text -> Maybe Text
nonEmpty value
    | Text.null (Text.strip value) = Nothing
    | otherwise = Just (Text.strip value)

{- | The transactions a request's query matches, with the key that identifies the
query to the checkpoint caches.

The key carries the reference date as well as the normalized text because the
text alone does not fix the query's meaning: it is resolved against
@latestJournalDate@, so the same text denotes a different span once an append
moves that date. The text is the normalized one actually parsed here, never the
raw request field.
-}
parseTransactions :: Snapshot -> Maybe Text -> AppM (QueryKey, [Hledger.Transaction])
parseTransactions snapshot requested = do
    let canonical = Text.strip (fromMaybe "" requested)
        reference = fromMaybe epoch snapshot.latestJournalDate
    parsed <- either throwError pure (parseHledgerQuery reference canonical)
    pure
        ( QueryKey{text = canonical, reference = reference}
        , filter (HledgerQuery.matchesTransaction parsed) snapshot.transactions
        )

-- * Read-model shaping (presentation, not calculation)

kpiFor :: Maybe Text -> (TimelinePoint -> Decimal) -> [TimelinePoint] -> Kpi
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
     in Kpi{value = currentValue, absoluteChange = change, percentageChange = percentage, trend = trendValues}

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

zeroTimeline :: TimelinePoint
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
     in CategoryBreakdown{period, total = sum (fmap (.quantity) grouped), items = grouped, otherItems}

groupOther :: [BreakdownItem] -> ([BreakdownItem], [BreakdownItem])
groupOther items =
    let sorted = List.sortOn (\item -> (negate item.quantity, item.account)) items
        total = sum (fmap (.quantity) sorted)
        visible item = total > 0 && item.quantity / total >= 0.01
        candidates = filter visible sorted
        named = take (if length candidates > 10 then 9 else 10) candidates
        namedAccounts = Set.fromList (fmap (.account) named)
        others = [item | item <- sorted, not (Set.member item.account namedAccounts)]
        other = sum (fmap (.quantity) others)
     in if other > 0
            then (named <> [BreakdownItem{account = "Other", quantity = other}], others)
            else (named, [])

toActivityBucket :: ActivityBucketFigures -> ActivityBucket
toActivityBucket figures =
    ActivityBucket
        { key = figures.key
        , startDate = figures.startDate
        , total = figures.total
        , increase = figures.increase
        , decrease = figures.decrease
        , reallocated = figures.reallocated
        , categories = fst (groupOther figures.categoryItems)
        , directions =
            [ DirectionBreakdown{direction, categories = fst (groupOther items)}
            | (direction, items) <- figures.directionItems
            ]
        , transactions = figures.transactions
        }

conversionWarnings :: Set Text -> [ConversionWarning]
conversionWarnings missing
    | Set.null missing = []
    | otherwise = [MissingConversion missing]
