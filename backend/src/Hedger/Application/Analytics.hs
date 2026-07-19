module Hedger.Application.Analytics (
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
    NetWorthReport (..),
    CashflowReport (..),
    ActivityBucket (..),
    ActivityReport (..),
    netWorth,
    cashflow,
    activity,
) where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Decimal (Decimal)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (
    Day,
    addDays,
    addGregorianMonthsClip,
    fromGregorian,
    toGregorian,
 )
import Text.Read (readMaybe)

import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Dates qualified as HledgerDates
import Hledger.Data.Period qualified as HledgerPeriod
import Hledger.Data.Types qualified as Hledger
import Hledger.Data.Valuation qualified as HledgerValuation
import Hledger.Query qualified as HledgerQuery
import Hledger.Reports.BalanceReport qualified as HledgerBalance
import Hledger.Reports.MultiBalanceReport qualified as HledgerMultiBalance
import Hledger.Reports.ReportOptions qualified as HledgerReportOptions
import Hledger.Reports.ReportTypes qualified as HledgerReportTypes

import Hedger.Application.AppM (AppM, Env (..), requireSnapshot)
import Hedger.Application.Common (SnapshotMetadata, metadataFor)
import Hedger.Application.TransactionQuery (parseHledgerQuery)
import Hedger.Config (Config (..))
import Hedger.Domain.Account qualified as Account
import Hedger.Domain.Error (AppError (..))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Transaction qualified as Transaction

data Granularity = Monthly | Yearly
    deriving stock (Eq, Show)

data AnalyticsRequest = AnalyticsRequest
    { query :: Maybe Text
    , granularity :: Granularity
    , displayCommodity :: Maybe Text
    , positivePeriod :: Maybe Text
    , negativePeriod :: Maybe Text
    , positiveDepth :: Int
    , negativeDepth :: Int
    }

data ActivityKind
    = IncomeActivity
    | ExpenseActivity
    | AssetActivity
    | LiabilityActivity
    deriving stock (Eq, Show)

data ActivityRange = MonthActivity | FiscalYearActivity
    deriving stock (Eq, Show)

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

data TimelinePoint = TimelinePoint
    { period :: Text
    , startDate :: Day
    , positive :: Decimal
    , negative :: Decimal
    , net :: Decimal
    }

data BreakdownItem = BreakdownItem
    { account :: Text
    , quantity :: Decimal
    }

data CategoryBreakdown = CategoryBreakdown
    { period :: Text
    , total :: Decimal
    , items :: [BreakdownItem]
    , otherItems :: [BreakdownItem]
    }

data PerformancePoint = PerformancePoint
    { period :: Text
    , startDate :: Day
    , netInvestment :: Decimal
    , netWorth :: Decimal
    , gainLoss :: Decimal
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
    , warnings :: [Text]
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
    , warnings :: [Text]
    }

data ActivityBucket = ActivityBucket
    { key :: Text
    , startDate :: Day
    , total :: Decimal
    , increase :: Decimal
    , decrease :: Decimal
    , reallocated :: Decimal
    , categories :: [BreakdownItem]
    , transactions :: [Hledger.Transaction]
    }

data ActivityReport = ActivityReport
    { metadata :: SnapshotMetadata
    , displayCommodity :: Text
    , kind :: ActivityKind
    , range :: ActivityRange
    , period :: Text
    , fiscalYearStartMonth :: Int
    , buckets :: [ActivityBucket]
    , warnings :: [Text]
    }

data ReportPeriod = ReportPeriod
    { key :: Text
    , start :: Day
    , end :: Day
    }

data Movement = Movement
    { date :: Day
    , account :: Text
    , quantity :: Decimal
    }

data Conversion value = Conversion
    { value :: Maybe value
    , missing :: Set Text
    }

instance Functor Conversion where
    fmap change conversion =
        Conversion
            { value = fmap change conversion.value
            , missing = conversion.missing
            }

netWorth :: AnalyticsRequest -> AppM NetWorthReport
netWorth request = do
    snapshot <- requireSnapshot
    commodity <- resolvedCommodity request.displayCommodity
    responseMetadata <- metadataFor snapshot
    matched <- parseTransactions snapshot request.query
    let assetReport =
            periodicBalanceReport
                snapshot
                matched
                request.granularity
                commodity
                [Hledger.Asset, Hledger.Cash]
                HledgerReportOptions.CalcChange
                HledgerReportOptions.Historical
                request.positiveDepth
        liabilityReport =
            periodicBalanceReport
                snapshot
                matched
                request.granularity
                commodity
                [Hledger.Liability]
                HledgerReportOptions.CalcChange
                HledgerReportOptions.Historical
                request.negativeDepth
        gainReport =
            periodicBalanceReport
                snapshot
                matched
                request.granularity
                commodity
                [Hledger.Asset, Hledger.Cash, Hledger.Liability]
                HledgerReportOptions.CalcGain
                HledgerReportOptions.Historical
                1
        periods = reportPeriodsFrom request.granularity assetReport
        assetTotals = reportTotals commodity assetReport
        liabilityTotals = fmap (fmap abs) (reportTotals commodity liabilityReport)
        points = zipWith3 netWorthPointFrom periods assetTotals liabilityTotals
        positivePeriod = selectedPeriod request.positivePeriod periods
        negativePeriod = selectedPeriod request.negativePeriod periods
        assetsBreakdown = reportBreakdown commodity request.positiveDepth positivePeriod periods assetReport
        liabilitiesBreakdown =
            reportBreakdown commodity request.negativeDepth negativePeriod periods liabilityReport
        performanceResults =
            performanceFrom commodity periods points assetTotals liabilityTotals gainReport
        performance = mapMaybe (.value) performanceResults
        performanceMissing = reportMissing commodity gainReport
        historyMissing = reportMissing commodity assetReport <> reportMissing commodity liabilityReport
        selectedKey = (.key) <$> selectedPeriod request.positivePeriod periods
        timelineNetWorth point = point.net
        timelineAssets point = point.positive
        timelineLiabilities point = point.negative
        historicalNetWorthKpi = kpiFor selectedKey timelineNetWorth points
        historicalAssetsKpi = kpiFor selectedKey timelineAssets points
        historicalLiabilitiesKpi = kpiFor selectedKey timelineLiabilities points
        currentAssets = currentReportValue snapshot matched commodity [Hledger.Asset, Hledger.Cash]
        currentLiabilities = fmap abs (currentReportValue snapshot matched commodity [Hledger.Liability])
        currentMissing = currentAssets.missing <> currentLiabilities.missing
        currentNetWorth = (-) <$> currentAssets.value <*> currentLiabilities.value
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
        netWorthKpi = useCurrent historicalNetWorthKpi currentNetWorth
        assetsKpi = useCurrent historicalAssetsKpi currentAssets.value
        liabilitiesKpi = useCurrent historicalLiabilitiesKpi currentLiabilities.value
        allMissing = historyMissing <> performanceMissing <> currentMissing
        reportWarnings = conversionWarnings allMissing
    pure
        NetWorthReport
            { metadata = responseMetadata
            , displayCommodity = commodity
            , granularity = request.granularity
            , netWorthKpi
            , assetsKpi
            , liabilitiesKpi
            , timeline = points
            , assetsBreakdown
            , liabilitiesBreakdown
            , performance
            , incompletePeriods = length (filter (not . Set.null . (.missing)) performanceResults)
            , warnings = reportWarnings
            }

cashflow :: AnalyticsRequest -> AppM CashflowReport
cashflow request = do
    snapshot <- requireSnapshot
    commodity <- resolvedCommodity request.displayCommodity
    responseMetadata <- metadataFor snapshot
    matched <- parseTransactions snapshot request.query
    let incomeReport =
            periodicBalanceReport
                snapshot
                matched
                request.granularity
                commodity
                [Hledger.Revenue, Hledger.Gain]
                HledgerReportOptions.CalcChange
                HledgerReportOptions.PerPeriod
                request.positiveDepth
        expenseReport =
            periodicBalanceReport
                snapshot
                matched
                request.granularity
                commodity
                [Hledger.Expense]
                HledgerReportOptions.CalcChange
                HledgerReportOptions.PerPeriod
                request.negativeDepth
        periods = reportPeriodsFrom request.granularity incomeReport
        incomeTotals = fmap (fmap negate) (reportTotals commodity incomeReport)
        expenseTotals = reportTotals commodity expenseReport
        missing = reportMissing commodity incomeReport <> reportMissing commodity expenseReport
        available = Set.null missing
        points =
            if available
                then zipWith3 cashflowPointFrom periods incomeTotals expenseTotals
                else []
        positivePeriod = selectedPeriod request.positivePeriod periods
        negativePeriod = selectedPeriod request.negativePeriod periods
        incomeBreakdown =
            if available
                then reportBreakdown commodity request.positiveDepth positivePeriod periods incomeReport
                else emptyBreakdown positivePeriod
        expenseBreakdown =
            if available
                then reportBreakdown commodity request.negativeDepth negativePeriod periods expenseReport
                else emptyBreakdown negativePeriod
    pure
        CashflowReport
            { metadata = responseMetadata
            , displayCommodity = commodity
            , granularity = request.granularity
            , totalCount = length snapshot.transactions
            , filteredCount = length matched
            , timelineAvailable = available
            , timeline = points
            , incomeBreakdown
            , expenseBreakdown
            , warnings = conversionWarnings missing
            }

activity :: ActivityRequest -> AppM ActivityReport
activity request = do
    snapshot <- requireSnapshot
    commodity <- resolvedCommodity request.displayCommodity
    fiscalStart <- asks (.config.fiscalYearStartMonth)
    responseMetadata <- metadataFor snapshot
    filtered <- case request.kind of
        IncomeActivity -> parseTransactions snapshot request.query
        ExpenseActivity -> parseTransactions snapshot request.query
        AssetActivity -> pure snapshot.transactions
        LiabilityActivity -> pure snapshot.transactions
    activityPeriod <-
        either throwError pure (parseActivityPeriod fiscalStart request.range request.period)
    let inRange =
            filter
                ( \transaction ->
                    transaction.tdate >= activityPeriod.start
                        && transaction.tdate <= activityPeriod.end
                )
                filtered
        results = fmap (activityContribution snapshot commodity request.kind) inRange
        contributions = mapMaybe (.value) results
        missing = Set.unions (fmap (.missing) results)
        bucketed =
            Map.fromListWith
                (<>)
                [ (activityBucketKey request.range contribution.date, [contribution])
                | contribution <- contributions
                ]
        buckets = fmap (toActivityBucket request.depth request.range) (Map.toAscList bucketed)
    pure
        ActivityReport
            { metadata = responseMetadata
            , displayCommodity = commodity
            , kind = request.kind
            , range = request.range
            , period = activityPeriod.key
            , fiscalYearStartMonth = fiscalStart
            , buckets
            , warnings = conversionWarnings missing
            }

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

epoch :: Day
epoch = fromGregorian 1970 1 1

periodicBalanceReport ::
    Snapshot ->
    [Hledger.Transaction] ->
    Granularity ->
    Text ->
    [Hledger.AccountType] ->
    HledgerReportOptions.BalanceCalculation ->
    HledgerReportOptions.BalanceAccumulation ->
    Int ->
    HledgerMultiBalance.MultiBalanceReport
periodicBalanceReport
    snapshot
    transactions
    granularity
    commodity
    accountTypes
    calculation
    accumulation
    depth =
        HledgerMultiBalance.multiBalanceReport reportSpec filteredJournal
      where
        (reportPeriod, reportInterval) = reportWindow granularity transactions snapshot.latestJournalDate
        valuation = case accumulation of
            HledgerReportOptions.PerPeriod -> HledgerValuation.AtThen (Just commodity)
            HledgerReportOptions.Cumulative -> HledgerValuation.AtEnd (Just commodity)
            HledgerReportOptions.Historical -> HledgerValuation.AtEnd (Just commodity)
        reportOptions =
            HledgerReportOptions.defreportopts
                { HledgerReportOptions.period_ = reportPeriod
                , HledgerReportOptions.interval_ = reportInterval
                , HledgerReportOptions.value_ = Just valuation
                , HledgerReportOptions.depth_ = Hledger.DepthSpec (Just (max 1 depth)) []
                , HledgerReportOptions.empty_ = True
                , HledgerReportOptions.balancecalc_ = calculation
                , HledgerReportOptions.balanceaccum_ = accumulation
                , HledgerReportOptions.accountlistmode_ = HledgerReportOptions.ALFlat
                }
        reportSpec =
            HledgerReportOptions.defreportspec
                { HledgerReportOptions._rsReportOpts = reportOptions
                , HledgerReportOptions._rsDay = fromMaybe epoch snapshot.latestJournalDate
                , HledgerReportOptions._rsQuery =
                    HledgerQuery.And
                        [ HledgerQuery.Type accountTypes
                        , HledgerQuery.Date (HledgerPeriod.periodAsDateSpan reportPeriod)
                        ]
                }
        filteredJournal = snapshot.journal{Hledger.jtxns = transactions}

reportWindow :: Granularity -> [Hledger.Transaction] -> Maybe Day -> (Hledger.Period, Hledger.Interval)
reportWindow granularity transactions latestDate =
    case latestDate <|> maximumMaybe (fmap (.tdate) transactions) of
        Nothing -> (Hledger.PeriodAll, interval)
        Just latest -> case granularity of
            Monthly ->
                let finalMonth = monthStart latest
                    firstMonth = addGregorianMonthsClip (-11) finalMonth
                 in (Hledger.PeriodBetween firstMonth (addGregorianMonthsClip 1 finalMonth), interval)
            Yearly ->
                let (finalYear, _, _) = toGregorian latest
                    firstYear = maybe finalYear yearOf (minimumMaybe (fmap (.tdate) transactions))
                 in ( Hledger.PeriodBetween
                        (fromGregorian firstYear 1 1)
                        (fromGregorian (finalYear + 1) 1 1)
                    , interval
                    )
  where
    interval = case granularity of
        Monthly -> Hledger.Months 1
        Yearly -> Hledger.Years 1
    left <|> right = case left of
        Just _ -> left
        Nothing -> right
    yearOf day = let (year, _, _) = toGregorian day in year

reportPeriodsFrom :: Granularity -> HledgerMultiBalance.MultiBalanceReport -> [ReportPeriod]
reportPeriodsFrom granularity report = fmap toPeriod report.prDates
  where
    toPeriod dateSpan =
        let start = fromMaybe epoch (HledgerDates.spanStart dateSpan)
            exclusiveEnd = fromMaybe (addDays 1 start) (HledgerDates.spanEnd dateSpan)
         in ReportPeriod
                { key = case granularity of
                    Monthly -> monthKey start
                    Yearly -> let (year, _, _) = toGregorian start in Text.pack (show year)
                , start
                , end = addDays (-1) exclusiveEnd
                }

reportTotals :: Text -> HledgerMultiBalance.MultiBalanceReport -> [Conversion Decimal]
reportTotals commodity report = fmap (conversionFromMixed commodity) report.prTotals.prrAmounts

reportMissing :: Text -> HledgerMultiBalance.MultiBalanceReport -> Set Text
reportMissing commodity report =
    Set.unions
        [ (conversionFromMixed commodity amount).missing
        | amount <- report.prTotals.prrAmounts <> concatMap (.prrAmounts) report.prRows
        ]

conversionFromMixed :: Text -> Hledger.MixedAmount -> Conversion Decimal
conversionFromMixed commodity amount =
    let projected = HledgerAmount.amountsRaw amount
        missing = Set.fromList [item.acommodity | item <- projected, item.acommodity /= commodity]
     in Conversion
            { value = Just (sum [item.aquantity | item <- projected, item.acommodity == commodity])
            , missing
            }

requireComplete :: Conversion value -> Conversion value
requireComplete conversion
    | Set.null conversion.missing = conversion
    | otherwise = Conversion{value = Nothing, missing = conversion.missing}

reportBreakdown ::
    Text ->
    Int ->
    Maybe ReportPeriod ->
    [ReportPeriod] ->
    HledgerMultiBalance.MultiBalanceReport ->
    CategoryBreakdown
reportBreakdown commodity depth selected periods report =
    case selected of
        Nothing -> CategoryBreakdown{period = "", total = 0, items = [], otherItems = []}
        Just period ->
            let periodIndex = fromMaybe 0 (List.findIndex ((== period.key) . (.key)) periods)
                raw =
                    mapMaybe
                        ( \row -> do
                            amount <- atIndex periodIndex row.prrAmounts
                            quantity <- (conversionFromMixed commodity amount).value
                            if abs quantity < 0.000001
                                then Nothing
                                else
                                    Just
                                        BreakdownItem
                                            { account = row.prrName.displayFull
                                            , quantity = abs quantity
                                            }
                        )
                        report.prRows
                depthGrouped =
                    [ BreakdownItem{account, quantity}
                    | (account, quantity) <-
                        Map.toList
                            ( Map.fromListWith
                                (+)
                                [(atDepth depth item.account, item.quantity) | item <- raw]
                            )
                    ]
                (grouped, otherItems) = groupOther depthGrouped
             in CategoryBreakdown
                    { period = period.key
                    , total = sum (fmap (.quantity) grouped)
                    , items = grouped
                    , otherItems
                    }

currentReportValue :: Snapshot -> [Hledger.Transaction] -> Text -> [Hledger.AccountType] -> Conversion Decimal
currentReportValue snapshot transactions commodity accountTypes =
    case snapshot.latestJournalDate of
        Nothing -> complete 0
        Just day ->
            let options =
                    HledgerReportOptions.defreportopts
                        { HledgerReportOptions.period_ = Hledger.PeriodTo (addDays 1 day)
                        , HledgerReportOptions.value_ = Just (HledgerValuation.AtDate day (Just commodity))
                        , HledgerReportOptions.balanceaccum_ = HledgerReportOptions.Historical
                        , HledgerReportOptions.accountlistmode_ = HledgerReportOptions.ALFlat
                        }
                spec =
                    HledgerReportOptions.defreportspec
                        { HledgerReportOptions._rsReportOpts = options
                        , HledgerReportOptions._rsDay = day
                        , HledgerReportOptions._rsQuery =
                            HledgerQuery.And
                                [ HledgerQuery.Type accountTypes
                                , HledgerQuery.Date
                                    (HledgerPeriod.periodAsDateSpan (Hledger.PeriodTo (addDays 1 day)))
                                ]
                        }
                filteredJournal = snapshot.journal{Hledger.jtxns = transactions}
                (_, total) = HledgerBalance.balanceReport spec filteredJournal
             in requireComplete (conversionFromMixed commodity total)

netWorthPointFrom :: ReportPeriod -> Conversion Decimal -> Conversion Decimal -> TimelinePoint
netWorthPointFrom period assetsResult liabilitiesResult =
    let assets = fromMaybe 0 assetsResult.value
        liabilities = fromMaybe 0 liabilitiesResult.value
     in TimelinePoint
            { period = period.key
            , startDate = period.start
            , positive = assets
            , negative = liabilities
            , net = assets - liabilities
            }

cashflowPointFrom :: ReportPeriod -> Conversion Decimal -> Conversion Decimal -> TimelinePoint
cashflowPointFrom period incomeResult expenseResult =
    let income = fromMaybe 0 incomeResult.value
        expenses = fromMaybe 0 expenseResult.value
     in TimelinePoint
            { period = period.key
            , startDate = period.start
            , positive = income
            , negative = expenses
            , net = income - expenses
            }

performanceFrom ::
    Text ->
    [ReportPeriod] ->
    [TimelinePoint] ->
    [Conversion Decimal] ->
    [Conversion Decimal] ->
    HledgerMultiBalance.MultiBalanceReport ->
    [Conversion PerformancePoint]
performanceFrom commodity periods netWorthPoints assetResults liabilityResults gainReport =
    zipWith3
        toPerformance
        periods
        netWorthPoints
        (zip3 assetResults liabilityResults (reportTotals commodity gainReport))
  where
    toPerformance period worthPoint (assetResult, liabilityResult, gainResult) =
        let missing = assetResult.missing <> liabilityResult.missing <> gainResult.missing
            point = do
                gain <- gainResult.value
                pure
                    PerformancePoint
                        { period = period.key
                        , startDate = period.start
                        , netInvestment = worthPoint.net - gain
                        , netWorth = worthPoint.net
                        , gainLoss = gain
                        }
         in Conversion{value = if Set.null missing then point else Nothing, missing}

monthStart :: Day -> Day
monthStart day =
    let (year, month, _) = toGregorian day
     in fromGregorian year month 1

monthPeriod :: Day -> ReportPeriod
monthPeriod day =
    let first = monthStart day
        next = addGregorianMonthsClip 1 first
     in ReportPeriod{key = monthKey first, start = first, end = addDays (-1) next}

monthKey :: Day -> Text
monthKey day =
    let (year, month, _) = toGregorian day
     in Text.pack (show year <> "-" <> pad2 month)

dayKey :: Day -> Text
dayKey = Text.pack . show

pad2 :: Int -> String
pad2 value = if value < 10 then "0" <> show value else show value

selectedPeriod :: Maybe Text -> [ReportPeriod] -> Maybe ReportPeriod
selectedPeriod requested periods =
    case requested >>= (\key -> List.find ((== key) . (.key)) periods) of
        Just period -> Just period
        Nothing -> lastMaybe periods

convertMixedAt :: Snapshot -> Text -> Day -> Hledger.MixedAmount -> Conversion Decimal
convertMixedAt snapshot commodity day amount =
    let valued = Account.valueAt (Account.mkValuationContext snapshot.journal commodity day) amount
        projected = HledgerAmount.amountsRaw valued
        missing = Set.fromList [item.acommodity | item <- projected, item.acommodity /= commodity]
     in Conversion
            { value = if Set.null missing then Just (sum (fmap (.aquantity) projected)) else Nothing
            , missing
            }

belongsTo :: Text -> Hledger.Posting -> Bool
belongsTo category posting = Account.accountLabel (Account.accountInfo posting.paccount) == category

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

percentageFrom :: Decimal -> Decimal -> Maybe Decimal
percentageFrom current previous
    | abs previous < 0.000001 = Nothing
    | otherwise = Just (((current - previous) / abs previous) * 100)

zeroTimeline :: TimelinePoint
zeroTimeline = TimelinePoint{period = "", startDate = epoch, positive = 0, negative = 0, net = 0}

emptyBreakdown :: Maybe ReportPeriod -> CategoryBreakdown
emptyBreakdown selected =
    CategoryBreakdown{period = maybe "" (.key) selected, total = 0, items = [], otherItems = []}

atDepth :: Int -> Text -> Text
atDepth depth account = Text.intercalate ":" (take (max 1 (min 4 depth)) (Text.splitOn ":" account))

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

categoryPostingMovements ::
    Snapshot ->
    Text ->
    Text ->
    Hledger.Transaction ->
    (Decimal -> Decimal) ->
    Conversion [Movement]
categoryPostingMovements snapshot commodity category transaction change =
    foldMapConversion
        [ fmap
            (Movement transaction.tdate posting.paccount . change)
            (convertMixedAt snapshot commodity transaction.tdate posting.pamount)
        | posting <- transaction.tpostings
        , belongsTo category posting
        ]

data ActivityContribution = ActivityContribution
    { date :: Day
    , total :: Decimal
    , increase :: Decimal
    , decrease :: Decimal
    , reallocated :: Decimal
    , categories :: [Movement]
    , transaction :: Hledger.Transaction
    }

activityContribution ::
    Snapshot ->
    Text ->
    ActivityKind ->
    Hledger.Transaction ->
    Conversion ActivityContribution
activityContribution snapshot commodity activityKind transaction =
    case (activityKind, Transaction.classificationFor transaction) of
        (IncomeActivity, Transaction.TransactionIncome) ->
            flowActivity snapshot commodity "income" negate transaction
        (ExpenseActivity, Transaction.TransactionExpense) ->
            flowActivity snapshot commodity "expenses" id transaction
        (AssetActivity, classification@Transaction.AssetAcquisition) ->
            assetActivity snapshot commodity classification transaction
        (AssetActivity, classification@Transaction.AssetDisposal) ->
            assetActivity snapshot commodity classification transaction
        (AssetActivity, classification@Transaction.AssetReallocation) ->
            assetActivity snapshot commodity classification transaction
        (LiabilityActivity, classification@Transaction.Borrowing) ->
            liabilityActivity snapshot commodity classification transaction
        (LiabilityActivity, classification@Transaction.Repayment) ->
            liabilityActivity snapshot commodity classification transaction
        _ -> completeNothing

flowActivity ::
    Snapshot ->
    Text ->
    Text ->
    (Decimal -> Decimal) ->
    Hledger.Transaction ->
    Conversion ActivityContribution
flowActivity snapshot commodity category change transaction =
    let movementsResult = categoryPostingMovements snapshot commodity category transaction change
        contribution = do
            movements <- movementsResult.value
            let total = sum (fmap (.quantity) movements)
            if abs total < 0.000001
                then Nothing
                else
                    Just
                        ActivityContribution
                            { date = transaction.tdate
                            , total
                            , increase = 0
                            , decrease = 0
                            , reallocated = 0
                            , categories = movements
                            , transaction
                            }
     in Conversion
            { value = if Set.null movementsResult.missing then contribution else Nothing
            , missing = movementsResult.missing
            }

assetActivity ::
    Snapshot ->
    Text ->
    Transaction.TransactionClassification ->
    Hledger.Transaction ->
    Conversion ActivityContribution
assetActivity snapshot commodity classification transaction =
    let results =
            [ fmap
                (\amount -> (posting, amount))
                (convertMixedAt snapshot commodity transaction.tdate posting.pamount)
            | posting <- transaction.tpostings
            , belongsTo "assets" posting
            ]
        missing = Set.unions (fmap (.missing) results)
        postings = mapMaybe (.value) results
        cash posting =
            Account.isCashOnly (Account.accountInfo posting.paccount)
                || inferredCashName posting.paccount
        totalWhere predicate = sum [abs amount | (posting, amount) <- postings, predicate posting amount]
        otherIn = totalWhere (\posting amount -> not (cash posting) && amount > 0)
        otherOut = totalWhere (\posting amount -> not (cash posting) && amount < 0)
        (acquired, disposed, realloc) = case classification of
            Transaction.AssetAcquisition -> (otherIn, 0, 0)
            Transaction.AssetDisposal -> (0, otherOut, 0)
            Transaction.AssetReallocation -> (0, 0, min otherIn otherOut)
            _ -> (0, 0, 0)
        categoryValues =
            [ Movement transaction.tdate posting.paccount (abs amount)
            | (posting, amount) <- postings
            , not (cash posting)
            , abs amount >= 0.000001
            ]
        relevant = acquired > 0 || disposed > 0 || realloc > 0
        result =
            if relevant && Set.null missing
                then
                    Just
                        ActivityContribution
                            { date = transaction.tdate
                            , total = acquired + disposed + realloc
                            , increase = acquired
                            , decrease = disposed
                            , reallocated = realloc
                            , categories = categoryValues
                            , transaction
                            }
                else Nothing
     in Conversion{value = result, missing}

liabilityActivity ::
    Snapshot ->
    Text ->
    Transaction.TransactionClassification ->
    Hledger.Transaction ->
    Conversion ActivityContribution
liabilityActivity snapshot commodity classification transaction =
    let results =
            [ fmap
                (Movement transaction.tdate posting.paccount)
                (convertMixedAt snapshot commodity transaction.tdate posting.pamount)
            | posting <- transaction.tpostings
            , belongsTo "liabilities" posting
            ]
        missing = Set.unions (fmap (.missing) results)
        movements = mapMaybe (.value) results
        borrowed =
            if classification == Transaction.Borrowing
                then sum [abs movement.quantity | movement <- movements, movement.quantity < 0]
                else 0
        repaid =
            if classification == Transaction.Repayment
                then sum [movement.quantity | movement <- movements, movement.quantity > 0]
                else 0
        relevant = borrowed > 0 || repaid > 0
        result =
            if relevant && Set.null missing
                then
                    Just
                        ActivityContribution
                            { date = transaction.tdate
                            , total = borrowed + repaid
                            , increase = borrowed
                            , decrease = repaid
                            , reallocated = 0
                            , categories = fmap absoluteMovement movements
                            , transaction
                            }
                else Nothing
     in Conversion{value = result, missing}

inferredCashName :: Text -> Bool
inferredCashName account =
    any
        (`elem` ["bank", "cash", "checking", "current", "deposit", "savings", "wallet"])
        (fmap Text.toLower (Text.splitOn ":" account))

absoluteMovement :: Movement -> Movement
absoluteMovement movement =
    Movement
        { date = movement.date
        , account = movement.account
        , quantity = abs movement.quantity
        }

parseActivityPeriod :: Int -> ActivityRange -> Text -> Either AppError ReportPeriod
parseActivityPeriod fiscalStart activityRange raw =
    case activityRange of
        MonthActivity ->
            case parseYearMonth raw of
                Just (year, month) -> Right (monthPeriod (fromGregorian year month 1))
                Nothing -> Left (InvalidRequest "Activity month must use YYYY-MM.")
        FiscalYearActivity ->
            case parseYearMonth raw of
                Just (year, month)
                    | month == fiscalStart ->
                        let first = fromGregorian year month 1
                            next = addGregorianMonthsClip 12 first
                         in Right
                                ReportPeriod
                                    { key = monthKey first
                                    , start = first
                                    , end = addDays (-1) next
                                    }
                _ ->
                    Left
                        ( InvalidRequest
                            ( "Fiscal activity period must be the configured fiscal-year start "
                                <> "month in YYYY-MM form."
                            )
                        )

parseYearMonth :: Text -> Maybe (Integer, Int)
parseYearMonth raw =
    case Text.splitOn "-" raw of
        [yearText, monthText] -> do
            year <- readText yearText
            month <- readText monthText
            if month >= 1 && month <= 12 && raw == Text.pack (show year <> "-" <> pad2 month)
                then Just (year, month)
                else Nothing
        _ -> Nothing

readText :: (Read value) => Text -> Maybe value
readText = readMaybe . Text.unpack

activityBucketKey :: ActivityRange -> Day -> Text
activityBucketKey MonthActivity = dayKey
activityBucketKey FiscalYearActivity = monthKey

toActivityBucket :: Int -> ActivityRange -> (Text, [ActivityContribution]) -> ActivityBucket
toActivityBucket depth activityRange (key, contributions) =
    let movements = concatMap (.categories) contributions
        startDate = case activityRange of
            MonthActivity -> fromMaybe epoch (readMaybe (Text.unpack key))
            FiscalYearActivity ->
                maybe epoch (\(year, month) -> fromGregorian year month 1) (parseYearMonth key)
        categoryTotals =
            Map.fromListWith
                (+)
                [ (atDepth depth movement.account, movement.quantity)
                | movement <- movements
                ]
        categoryItems =
            [ BreakdownItem{account, quantity = abs quantity}
            | (account, quantity) <- Map.toList categoryTotals
            ]
     in ActivityBucket
            { key
            , startDate
            , total = sum (fmap (.total) contributions)
            , increase = sum (fmap (.increase) contributions)
            , decrease = sum (fmap (.decrease) contributions)
            , reallocated = sum (fmap (.reallocated) contributions)
            , categories = fst (groupOther categoryItems)
            , transactions = fmap (.transaction) contributions
            }

conversionWarnings :: Set Text -> [Text]
conversionWarnings missing
    | Set.null missing = []
    | otherwise =
        [ "Missing dated conversion to the display commodity for: "
            <> Text.intercalate ", " (Set.toAscList missing)
        ]

complete :: value -> Conversion value
complete value = Conversion{value = Just value, missing = Set.empty}

completeNothing :: Conversion value
completeNothing = Conversion{value = Nothing, missing = Set.empty}

foldMapConversion :: [Conversion value] -> Conversion [value]
foldMapConversion conversions =
    let missing = Set.unions (fmap (.missing) conversions)
     in Conversion
            { value = Just (mapMaybe (.value) conversions)
            , missing
            }

maximumMaybe :: (Ord value) => [value] -> Maybe value
maximumMaybe [] = Nothing
maximumMaybe values = Just (maximum values)

minimumMaybe :: (Ord value) => [value] -> Maybe value
minimumMaybe [] = Nothing
minimumMaybe values = Just (minimum values)

lastMaybe :: [value] -> Maybe value
lastMaybe [] = Nothing
lastMaybe values = Just (last values)

atIndex :: Int -> [value] -> Maybe value
atIndex index values
    | index < 0 = Nothing
    | otherwise = case drop index values of
        value : _ -> Just value
        [] -> Nothing
