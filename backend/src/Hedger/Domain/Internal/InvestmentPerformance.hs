module Hedger.Domain.Internal.InvestmentPerformance (
    -- * Value objects
    InvestmentCompleteness (..),
    InvestmentSummary (..),
    InvestmentWarning (..),
    WarningKind (..),
    SummaryResult (..),
    InvestmentPerformanceContext (..),

    -- * Performance calculation
    prepareInvestmentPerformance,
    summaryAtPrepared,
    summariesAtPrepared,
    summaryAt,
    investmentSummaryAt,
    emptySummary,

    -- * Ledger policies and derivations
    validateScope,
    isInvestmentAccount,
    investmentAccountsAt,
    investmentDataTransactions,
    immediateChildren,
    childrenOf,
    reportPeriods,

    -- * Building blocks (exposed for property tests)
    holdingsAt,
    holdingsAtWithJournal,
    holdingsSeriesAtWithJournal,
    journalWithHistoricalCosts,
    investmentQuery,
    attributedAt,
    attributedAtWith,
    retainedPerformanceAt,
    retainedPerformanceAtWith,
    hasAttributablePerformance,
    accountUsesExplicitCost,
    externalCashFlowsAt,
    externalCashFlowsAtWith,
    journalWithHistoricalCostsUsing,
    ScopeScan (..),
    scopeScansAt,
)
where

import Data.Decimal (Decimal)
import Data.List qualified as List
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, addDays)

import Hledger.Data.AccountName qualified as HledgerAccountName
import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Dates qualified as HledgerDates
import Hledger.Data.DayPartition qualified as HledgerDayPartition
import Hledger.Data.Journal qualified as HledgerJournal
import Hledger.Data.Period qualified as HledgerPeriod
import Hledger.Data.Types qualified as Hledger
import Hledger.Data.Valuation qualified as HledgerValuation
import Hledger.Query qualified as HledgerQuery
import Hledger.Reports.BalanceReport qualified as HledgerBalance
import Hledger.Reports.MultiBalanceReport qualified as HledgerMultiBalance
import Hledger.Reports.ReportOptions qualified as HledgerReportOptions
import Hledger.Reports.ReportTypes qualified as HledgerReportTypes

import Hedger.Domain.Error (AppError (..))
import Hedger.Domain.Investment (accountWithin)
import Hedger.Domain.Investment qualified as Investment
import Hedger.Domain.Period (
    Granularity (..),
    ReportPeriod (..),
    epoch,
    intervalFor,
    maximumMaybe,
    minimumMaybe,
    periodsFromDateSpan,
    takeLast,
 )
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (completeDecimal, missingCommodities, valueMixedAt)
import Hedger.Domain.Xirr qualified as Xirr

data InvestmentCompleteness = Complete | Partial | Unavailable
    deriving stock (Eq, Show)

data InvestmentSummary = InvestmentSummary
    { marketValue :: Maybe Decimal
    , purchaseCost :: Maybe Decimal
    , gainLoss :: Maybe Decimal
    , xirr :: Maybe Decimal
    , attributedIncome :: Maybe Decimal
    , realisedGain :: Maybe Decimal
    , totalProfit :: Maybe Decimal
    , completeness :: InvestmentCompleteness
    }

{- | A data-quality condition detected while computing an investment summary.
The wording shown to a user is a boundary concern; the domain only records the
structured fact.
-}
data InvestmentWarning = InvestmentWarning
    { kind :: WarningKind
    , account :: Maybe Text
    }
    deriving stock (Eq, Show)

-- | The kind of data-quality gap, carrying the commodity/symbol it concerns.
data WarningKind
    = -- | No display-commodity price for the symbol (display commodity, symbol).
      MissingPrice Text Text
    | -- | No complete acquisition cost for the symbol.
      MissingCost Text
    | -- | An investment account mapping is missing, so income/gains are unavailable.
      MissingPerformanceMapping
    | -- | Acquisition cost is in a foreign commodity (display commodity, symbol).
      ForeignCost Text Text
    | -- | The XIRR could not be resolved to a single, convergent rate.
      UnresolvedReturn
    deriving stock (Eq, Show)

data SummaryResult = SummaryResult
    { summary :: InvestmentSummary
    , warnings :: [InvestmentWarning]
    }

{- | Journal- and configuration-derived state shared by every summary in one
investment report. Building this once is important: price-oracle construction
and historical-cost conversion otherwise repeat for every timeline point and
drill-down summary.
-}
data InvestmentPerformanceContext = InvestmentPerformanceContext
    { snapshot :: Snapshot
    , assetClasses :: Investment.AssetClassMappings
    , mappings :: Investment.InvestmentMappings
    , commodity :: Text
    , valueAt :: Day -> Hledger.MixedAmount -> Hledger.MixedAmount
    , historicalCostJournal :: Hledger.Journal
    , sortedTransactions :: [Hledger.Transaction]
    {- ^ hledger's @journalFinalise@ restores parse order via @journalReverse@ and
    never sorts by date, so a chronological sweep has to sort first. Sorted here
    rather than in 'scopeScansAt' because every scope in one request sweeps the
    same transactions. The sort is stable, so same-date transactions keep their
    file order; the resulting date-ordered 'cashFlows' is therefore a reordering
    of what the per-day functions produce, which is unobservable because its only
    consumer is 'Xirr.xirr', and 'Xirr.normalise' groups flows by day first.
    -}
    }

{- | The scope-derived, non-holdings inputs to one day's 'InvestmentSummary':
account discovery, mapping completeness, and the three quantities that each
require a full prefix scan of the journal. 'scopeScansAt' computes all four in
one chronological sweep instead of one scan per quantity per day.
-}
data ScopeScan = ScopeScan
    { accounts :: [Text]
    , mappingComplete :: Bool
    , retained :: Maybe Decimal
    , attributed :: Maybe (Decimal, Decimal, Decimal)
    , cashFlows :: Maybe [Xirr.CashFlow]
    }
    deriving stock (Eq, Show)

prepareInvestmentPerformance ::
    Snapshot ->
    Investment.AssetClassMappings ->
    Investment.InvestmentMappings ->
    Text ->
    InvestmentPerformanceContext
prepareInvestmentPerformance snapshot assetClasses mappings commodity = context
  where
    priceOracle = HledgerValuation.journalPriceOracle False snapshot.journal
    commodityStyles = HledgerJournal.journalCommodityStyles snapshot.journal
    valueAt day =
        HledgerValuation.mixedAmountValueAtDate
            priceOracle
            commodityStyles
            (Just commodity)
            day
    context =
        InvestmentPerformanceContext
            { snapshot
            , assetClasses
            , mappings
            , commodity
            , valueAt
            , historicalCostJournal = journalWithHistoricalCostsUsing valueAt snapshot commodity
            , sortedTransactions = List.sortOn (.tdate) snapshot.transactions
            }

summaryAt ::
    Snapshot ->
    Investment.AssetClassMappings ->
    Investment.InvestmentMappings ->
    Text ->
    Maybe Text ->
    Day ->
    SummaryResult
summaryAt snapshot assetClasses mappings commodity scope day =
    summaryAtPrepared
        (prepareInvestmentPerformance snapshot assetClasses mappings commodity)
        scope
        day

summaryAtPrepared :: InvestmentPerformanceContext -> Maybe Text -> Day -> SummaryResult
summaryAtPrepared context scope day =
    case scopeScansAt context scope [day] of
        [scopeScan] ->
            summaryFromHoldings
                context
                scope
                day
                (holdingsAt context.snapshot context.assetClasses scope day)
                (holdingsAtWithJournal context.historicalCostJournal context.assetClasses scope day)
                scopeScan
        _ -> error "scopeScansAt: expected exactly one ScopeScan for one requested day"

{- | Every summary in one investment timeline, computed with two bulk
'holdingsSeriesAtWithJournal' calls instead of two 'balanceReport's per day, and
one 'scopeScansAt' sweep instead of four journal scans per day.
-}
summariesAtPrepared ::
    InvestmentPerformanceContext -> Maybe Text -> Granularity -> [Day] -> [SummaryResult]
summariesAtPrepared context scope granularity days =
    List.zipWith4
        (summaryFromHoldings context scope)
        days
        (holdingsSeriesAtWithJournal context.snapshot.journal context.assetClasses scope granularity days)
        ( holdingsSeriesAtWithJournal
            context.historicalCostJournal
            context.assetClasses
            scope
            granularity
            days
        )
        (scopeScansAt context scope days)

-- | The per-day summary calculation, shared by the single-day and bulk entry points.
summaryFromHoldings ::
    InvestmentPerformanceContext ->
    Maybe Text ->
    Day ->
    Hledger.MixedAmount ->
    Hledger.MixedAmount ->
    ScopeScan ->
    SummaryResult
summaryFromHoldings context scope day holdings costHoldings scopeScan =
    let commodity = context.commodity
        costRaw = HledgerAmount.mixedAmountCost costHoldings
        valueRaw = context.valueAt day holdings
        costValued = context.valueAt day costRaw
        hasHoldings = any ((>= 0.000001) . abs . (.aquantity)) (HledgerAmount.amountsRaw holdings)
        market = completeDecimal commodity valueRaw
        rawMissingCost =
            Set.fromList
                [ item.acommodity
                | item <- HledgerAmount.amountsRaw costHoldings
                , abs item.aquantity >= 0.000001
                , item.acommodity /= commodity
                , item.acost == Nothing
                ]
        foreignCost =
            Set.fromList
                [ item.acommodity
                | item <- HledgerAmount.amountsRaw costRaw
                , abs item.aquantity >= 0.000001
                , item.acommodity /= commodity
                ]
        reportedCost =
            if Set.null rawMissingCost && Set.null foreignCost
                then completeDecimal commodity costValued
                else Nothing
        relevantAccounts = scopeScan.accounts
        mappingComplete = scopeScan.mappingComplete
        retained =
            if mappingComplete
                then scopeScan.retained
                else Just 0
        cost = (-) <$> reportedCost <*> retained
        calculatedGain = (-) <$> market <*> reportedCost
        attributed =
            if mappingComplete
                then scopeScan.attributed
                else Nothing
        income = attributed >>= (Just . (\(value, _, _) -> value))
        realised = attributed >>= (Just . (\(_, value, _) -> value))
        fees = attributed >>= (Just . (\(_, _, value) -> value))
        total = (+) <$> calculatedGain <*> ((+) <$> ((+) <$> income <*> realised) <*> fees)
        xirrResult =
            if mappingComplete
                then xirrFromCashFlows scopeScan.cashFlows day market
                else Nothing
        annualisedReturn = xirrResult >>= either (const Nothing) Just
        unresolvedReturn = maybe False (either isNonTrivialXirrError (const False)) xirrResult
        missingValue = missingCommodities commodity valueRaw
        missingCost = missingCommodities commodity costValued <> rawMissingCost
        warnings =
            [ InvestmentWarning (MissingPrice commodity symbol) Nothing
            | symbol <- Set.toList missingValue
            ]
                <> [ InvestmentWarning (MissingCost symbol) Nothing
                   | symbol <- Set.toList missingCost
                   ]
                <> [ InvestmentWarning MissingPerformanceMapping scope
                   | not (null relevantAccounts) && not mappingComplete
                   ]
                <> [ InvestmentWarning (ForeignCost commodity symbol) scope
                   | symbol <- Set.toList foreignCost
                   ]
                <> [ InvestmentWarning UnresolvedReturn scope
                   | unresolvedReturn
                   ]
        complete =
            if market == Nothing && cost == Nothing
                then Unavailable
                else if null warnings && total /= Nothing then Complete else Partial
     in if not hasHoldings
            then SummaryResult{summary = emptySummary, warnings = []}
            else
                SummaryResult
                    { summary =
                        InvestmentSummary
                            { marketValue = market
                            , purchaseCost = cost
                            , gainLoss = calculatedGain
                            , xirr = annualisedReturn
                            , attributedIncome = income
                            , realisedGain = realised
                            , totalProfit = total
                            , completeness = complete
                            }
                    , warnings
                    }

-- | Running totals threaded through 'scopeScansAt''s chronological sweep.
data Accumulator = Accumulator
    { seenAccounts :: !(Set.Set Text)
    , explicitCostAccounts :: !(Set.Set Text)
    , retainedTotal :: !(Maybe Decimal)
    , attributedTotal :: !(Maybe (Decimal, Decimal, Decimal))
    , -- Newest first: 'step' conses, and 'snapshotAt' reverses once per emitted day.
      cashFlowsTotal :: !(Maybe [Xirr.CashFlow])
    }

initialAccumulator :: Accumulator
initialAccumulator =
    Accumulator
        { seenAccounts = Set.empty
        , explicitCostAccounts = Set.empty
        , retainedTotal = Just 0
        , attributedTotal = Just (0, 0, 0)
        , cashFlowsTotal = Just []
        }

{- | 'ScopeScan' for each requested day, computed together in one chronological
sweep of 'context.snapshot.transactions' rather than the four independent
prefix scans ('investmentAccountsAt', 'hasAttributablePerformance' per
account, 'retainedPerformanceAtWith', 'attributedAtWith', and
'externalCashFlowsAtWith') that 'summaryFromHoldings' used to run per day. Each
accumulator mirrors its per-day counterpart's guards exactly; see that
function for the semantics being reproduced here incrementally.
-}
scopeScansAt :: InvestmentPerformanceContext -> Maybe Text -> [Day] -> [ScopeScan]
scopeScansAt context scope days = fmap snd (List.sortOn fst indexedResults)
  where
    commodity = context.commodity
    mappings = context.mappings
    valueAt = context.valueAt

    indexedDays = List.sortOn snd (zip [0 :: Int ..] days)
    indexedResults = walk initialAccumulator context.sortedTransactions indexedDays

    selectedMappings =
        filter
            (\mapping -> Investment.assetClassScopeOverlaps mapping.assetAccount context.assetClasses)
            ( case scope of
                Nothing -> Investment.mappingsForScope "" mappings
                Just selected -> Investment.mappingsForScope selected mappings
            )

    mappedAssetScope mapping = case scope of
        Just selected
            | mapping.assetAccount `accountWithin` selected -> selected
        _ -> mapping.assetAccount

    incomeLike role = case role of
        Investment.Interest -> True
        Investment.Dividend -> True
        Investment.OtherIncome -> True
        Investment.RealisedGain -> False
        Investment.Fee -> False

    touchesPortfolio :: Hledger.Transaction -> Bool
    touchesPortfolio =
        any (isInvestmentAccount context.assetClasses scope . (.paccount)) . (.tpostings)

    transactionHasSelectedPerformance transaction =
        any
            ( \mapping ->
                transactionMatchesScope scope mapping transaction
                    && any
                        ( \performance ->
                            any
                                (\posting -> performance.account `accountWithin` posting.paccount)
                                transaction.tpostings
                        )
                        mapping.performanceAccounts
            )
            selectedMappings

    isSelectedPerformancePosting transaction posting =
        any
            ( \mapping ->
                transactionMatchesScope scope mapping transaction
                    && any
                        (\performance -> performance.account `accountWithin` posting.paccount)
                        mapping.performanceAccounts
            )
            selectedMappings

    valuePostings transaction =
        fmap sum
            . traverse (completeDecimal commodity . valueAt transaction.tdate . (.pamount))

    transactionCashFlow transaction = do
        externalMovement <-
            valuePostings
                transaction
                [ posting
                | posting <- transaction.tpostings
                , not (isInvestmentAccount context.assetClasses scope posting.paccount)
                , not (isSelectedPerformancePosting transaction posting)
                ]
        pure (Xirr.cashFlow transaction.tdate externalMovement)

    -- Mirrors retainedPerformanceAtWith's list comprehension for one transaction.
    retainedContributions transaction
        | not (touchesPortfolio transaction) = []
        | otherwise =
            [ fmap (max 0) (completeDecimal commodity (valueAt transaction.tdate posting.pamount))
            | mapping <- selectedMappings
            , any
                ( \performance ->
                    incomeLike performance.role
                        && any
                            (\candidate -> performance.account `accountWithin` candidate.paccount)
                            transaction.tpostings
                )
                mapping.performanceAccounts
            , posting <- transaction.tpostings
            , isInvestmentAccount context.assetClasses scope posting.paccount
            , mappedAssetScope mapping `accountWithin` posting.paccount
            ]

    -- Mirrors attributedAtWith's list comprehension for one transaction.
    attributedContributions transaction
        | not (touchesPortfolio transaction) = []
        | otherwise =
            [ do
                quantity <- completeDecimal commodity (valueAt transaction.tdate posting.pamount)
                pure $ case performance.role of
                    Investment.RealisedGain -> (0, negate quantity, 0)
                    Investment.Fee -> (0, 0, negate quantity)
                    _ -> (negate quantity, 0, 0)
            | mapping <- selectedMappings
            , transactionMatchesScope scope mapping transaction
            , posting <- transaction.tpostings
            , performance <- mapping.performanceAccounts
            , performance.account `accountWithin` posting.paccount
            ]

    hasExplicitCost amount = abs amount.aquantity >= 0.000001 && amount.acost /= Nothing

    addSticky Nothing _ = Nothing
    addSticky _ Nothing = Nothing
    addSticky (Just total) (Just value) = Just (total + value)

    combineTriple Nothing _ = Nothing
    combineTriple _ Nothing = Nothing
    combineTriple (Just (incomeA, gainA, feeA)) (Just (incomeB, gainB, feeB)) =
        Just (incomeA + incomeB, gainA + gainB, feeA + feeB)

    step :: Accumulator -> Hledger.Transaction -> Accumulator
    step acc transaction =
        acc
            { seenAccounts = List.foldl' addAccount acc.seenAccounts transaction.tpostings
            , explicitCostAccounts =
                List.foldl' addExplicitCost acc.explicitCostAccounts transaction.tpostings
            , retainedTotal = List.foldl' addSticky acc.retainedTotal (retainedContributions transaction)
            , attributedTotal =
                List.foldl' combineTriple acc.attributedTotal (attributedContributions transaction)
            , cashFlowsTotal = addCashFlow acc.cashFlowsTotal transaction
            }
      where
        addAccount accounts posting
            | isInvestmentAccount context.assetClasses scope posting.paccount =
                Set.insert posting.paccount accounts
            | otherwise = accounts
        addExplicitCost accounts posting
            | any hasExplicitCost (HledgerAmount.amountsRaw posting.pamount) =
                Set.insert posting.paccount accounts
            | otherwise = accounts
        addCashFlow Nothing _ = Nothing
        addCashFlow (Just flows) currentTransaction
            | not
                ( touchesPortfolio currentTransaction
                    || transactionHasSelectedPerformance currentTransaction
                ) =
                Just flows
            | otherwise = case transactionCashFlow currentTransaction of
                Nothing -> Nothing
                Just flow -> Just (flow : flows)

    snapshotAt :: Accumulator -> ScopeScan
    snapshotAt acc =
        ScopeScan
            { accounts = Set.toList acc.seenAccounts
            , mappingComplete =
                not (Set.null acc.seenAccounts)
                    && all
                        ( \account ->
                            isJust (Investment.mappingForAccount account mappings)
                                || Set.member account acc.explicitCostAccounts
                        )
                        (Set.toList acc.seenAccounts)
            , retained = acc.retainedTotal
            , attributed = acc.attributedTotal
            , cashFlows = fmap reverse acc.cashFlowsTotal
            }

    walk :: Accumulator -> [Hledger.Transaction] -> [(Int, Day)] -> [(Int, ScopeScan)]
    walk _ _ [] = []
    walk acc [] pending = [(index, snapshotAt acc) | (index, _) <- pending]
    walk acc allTransactions@(transaction : restTransactions) pending@((index, day) : restPending)
        | transaction.tdate <= day = walk (step acc transaction) restTransactions pending
        | otherwise = (index, snapshotAt acc) : walk acc allTransactions restPending

investmentSummaryAt ::
    Snapshot ->
    Investment.AssetClassMappings ->
    Investment.InvestmentMappings ->
    Text ->
    Maybe Text ->
    Day ->
    InvestmentSummary
investmentSummaryAt snapshot assetClasses mappings commodity scope day =
    (summaryAt snapshot assetClasses mappings commodity scope day).summary

holdingsAt :: Snapshot -> Investment.AssetClassMappings -> Maybe Text -> Day -> Hledger.MixedAmount
holdingsAt snapshot = holdingsAtWithJournal snapshot.journal

holdingsAtWithJournal ::
    Hledger.Journal ->
    Investment.AssetClassMappings ->
    Maybe Text ->
    Day ->
    Hledger.MixedAmount
holdingsAtWithJournal journal assetClasses scope day =
    snd (HledgerBalance.balanceReport (spec scope) journal)
  where
    period = Hledger.PeriodTo (addDays 1 day)
    options =
        HledgerReportOptions.defreportopts
            { HledgerReportOptions.period_ = period
            , HledgerReportOptions.balanceaccum_ = HledgerReportOptions.Historical
            , HledgerReportOptions.accountlistmode_ = HledgerReportOptions.ALFlat
            }
    spec selected =
        HledgerReportOptions.defreportspec
            { HledgerReportOptions._rsReportOpts = options
            , HledgerReportOptions._rsDay = day
            , HledgerReportOptions._rsQuery =
                HledgerQuery.And
                    [ investmentQuery assetClasses selected
                    , HledgerQuery.Date (HledgerPeriod.periodAsDateSpan period)
                    ]
            }

{- | The cumulative holdings at each requested day, in one 'HledgerMultiBalance.multiBalanceReport'
rather than one 'HledgerBalance.balanceReport' per day.
-}
holdingsSeriesAtWithJournal ::
    Hledger.Journal ->
    Investment.AssetClassMappings ->
    Maybe Text ->
    Granularity ->
    [Day] ->
    [Hledger.MixedAmount]
holdingsSeriesAtWithJournal journal assetClasses scope granularity days =
    case (minimumMaybe days, maximumMaybe days) of
        (Just earliest, Just latest) ->
            let periodStart = HledgerDayPartition.intervalBoundaryBefore (intervalFor granularity) earliest
                period = Hledger.PeriodBetween periodStart (addDays 1 latest)
                options =
                    HledgerReportOptions.defreportopts
                        { HledgerReportOptions.period_ = period
                        , HledgerReportOptions.interval_ = intervalFor granularity
                        , HledgerReportOptions.balanceaccum_ = HledgerReportOptions.Historical
                        , HledgerReportOptions.accountlistmode_ = HledgerReportOptions.ALFlat
                        , -- Without this, hledger drops columns for periods with no activity,
                          -- and the series would lose entries for those requested days.
                          HledgerReportOptions.empty_ = True
                        }
                spec =
                    HledgerReportOptions.defreportspec
                        { HledgerReportOptions._rsReportOpts = options
                        , HledgerReportOptions._rsDay = latest
                        , HledgerReportOptions._rsQuery =
                            HledgerQuery.And
                                [ investmentQuery assetClasses scope
                                , HledgerQuery.Date (HledgerPeriod.periodAsDateSpan period)
                                ]
                        }
                report = HledgerMultiBalance.multiBalanceReport spec journal
                columns = zip report.prDates report.prTotals.prrAmounts
                -- Requested days don't correspond positionally to report columns (a clamped
                -- final period, or a day predating any data), so look up by span membership.
                containsDay day (columnSpan, _) = HledgerDates.spanContainsDate columnSpan day
                amountFor day =
                    case List.find (containsDay day) columns of
                        Just (_, amount) -> amount
                        Nothing -> mempty
             in fmap amountFor days
        _ -> []

journalWithHistoricalCosts :: Snapshot -> Text -> Hledger.Journal
journalWithHistoricalCosts snapshot commodity =
    journalWithHistoricalCostsUsing (valueMixedAt snapshot commodity) snapshot commodity

journalWithHistoricalCostsUsing ::
    (Day -> Hledger.MixedAmount -> Hledger.MixedAmount) ->
    Snapshot ->
    Text ->
    Hledger.Journal
journalWithHistoricalCostsUsing valueAt snapshot commodity =
    snapshot.journal
        { Hledger.jtxns = convertTransaction <$> snapshot.journal.jtxns
        }
  where
    convertTransaction transaction =
        transaction
            { Hledger.tpostings = convertPosting transaction.tdate <$> transaction.tpostings
            }
    convertPosting day posting =
        posting
            { Hledger.pamount =
                HledgerAmount.mapMixedAmount
                    (convertAmount day posting.paccount)
                    posting.pamount
            }
    convertAmount day account amount =
        amount
            { Hledger.acost =
                case amount.acost of
                    Just acquisitionCost -> Just (convertCost day acquisitionCost)
                    Nothing -> inferredCashCost day account amount
            }
    convertCost day acquisitionCost = case acquisitionCost of
        Hledger.UnitCost amount -> Hledger.UnitCost (convertCostAmount day amount)
        Hledger.TotalCost amount -> Hledger.TotalCost (convertCostAmount day amount)
    convertCostAmount day amount =
        fromMaybe
            amount
            ( HledgerAmount.unifyMixedAmount
                (valueAt day (HledgerAmount.mixedAmount amount))
            )
    inferredCashCost day account amount
        | "assets:cash" `accountWithin` account
            && amount.acommodity /= commodity =
            Hledger.UnitCost <$> historicalUnitPrice day amount
        | otherwise = Nothing
    historicalUnitPrice day amount = do
        valued <-
            HledgerAmount.unifyMixedAmount
                ( valueAt
                    day
                    ( HledgerAmount.mixedAmount
                        amount
                            { Hledger.aquantity = 1
                            , Hledger.acost = Nothing
                            }
                    )
                )
        if valued.acommodity == commodity then Just valued else Nothing

investmentQuery :: Investment.AssetClassMappings -> Maybe Text -> HledgerQuery.Query
investmentQuery assetClasses scope =
    HledgerQuery.And
        ( [ HledgerQuery.Type [Hledger.Asset]
          , configuredAccounts
          ]
            <> scopeQuery
        )
  where
    configuredAccounts =
        case [ HledgerQuery.Acct (HledgerAccountName.accountNameToAccountRegex prefix)
             | mapping <- Investment.assetClassMappingsList assetClasses
             , prefix <- mapping.accountPrefixes
             ] of
            [] -> HledgerQuery.None
            queries -> HledgerQuery.Or queries
    scopeQuery =
        maybe
            []
            (\name -> [HledgerQuery.Acct (HledgerAccountName.accountNameToAccountRegex name)])
            scope

attributedAt ::
    Snapshot ->
    Investment.AssetClassMappings ->
    Investment.InvestmentMappings ->
    Text ->
    Maybe Text ->
    Day ->
    Maybe (Decimal, Decimal, Decimal)
attributedAt snapshot assetClasses mappings commodity scope day =
    attributedAtWith (valueMixedAt snapshot commodity) snapshot assetClasses mappings commodity scope day

attributedAtWith ::
    (Day -> Hledger.MixedAmount -> Hledger.MixedAmount) ->
    Snapshot ->
    Investment.AssetClassMappings ->
    Investment.InvestmentMappings ->
    Text ->
    Maybe Text ->
    Day ->
    Maybe (Decimal, Decimal, Decimal)
attributedAtWith valueAt snapshot assetClasses mappings commodity scope day =
    foldl combine (Just (0, 0, 0)) contributions
  where
    selectedMappings =
        filter
            (\mapping -> Investment.assetClassScopeOverlaps mapping.assetAccount assetClasses)
            ( case scope of
                Nothing -> Investment.mappingsForScope "" mappings
                Just selected -> Investment.mappingsForScope selected mappings
            )
    contributions =
        [ do
            quantity <-
                completeDecimal
                    commodity
                    (valueAt transaction.tdate posting.pamount)
            pure $ case performance.role of
                Investment.RealisedGain -> (0, negate quantity, 0)
                Investment.Fee -> (0, 0, negate quantity)
                _ -> (negate quantity, 0, 0)
        | transaction <- snapshot.transactions
        , transaction.tdate <= day
        , transactionTouchesPortfolio transaction
        , mapping <- selectedMappings
        , transactionMatchesScope scope mapping transaction
        , posting <- transaction.tpostings
        , performance <- mapping.performanceAccounts
        , performance.account `accountWithin` posting.paccount
        ]
    combine Nothing _ = Nothing
    combine _ Nothing = Nothing
    combine (Just (incomeA, gainA, feeA)) (Just (incomeB, gainB, feeB)) =
        Just (incomeA + incomeB, gainA + gainB, feeA + feeB)
    transactionTouchesPortfolio =
        any (isInvestmentAccount assetClasses scope . (.paccount)) . (.tpostings)

retainedPerformanceAt ::
    Snapshot ->
    Investment.AssetClassMappings ->
    Investment.InvestmentMappings ->
    Text ->
    Maybe Text ->
    Day ->
    Maybe Decimal
retainedPerformanceAt snapshot assetClasses mappings commodity scope day =
    retainedPerformanceAtWith
        (valueMixedAt snapshot commodity)
        snapshot
        assetClasses
        mappings
        commodity
        scope
        day

retainedPerformanceAtWith ::
    (Day -> Hledger.MixedAmount -> Hledger.MixedAmount) ->
    Snapshot ->
    Investment.AssetClassMappings ->
    Investment.InvestmentMappings ->
    Text ->
    Maybe Text ->
    Day ->
    Maybe Decimal
retainedPerformanceAtWith valueAt snapshot assetClasses mappings commodity scope day =
    fmap sum (traverse id retainedAmounts)
  where
    selectedMappings =
        filter
            (\mapping -> Investment.assetClassScopeOverlaps mapping.assetAccount assetClasses)
            ( case scope of
                Nothing -> Investment.mappingsForScope "" mappings
                Just selected -> Investment.mappingsForScope selected mappings
            )
    retainedAmounts =
        [ fmap
            (max 0)
            (completeDecimal commodity (valueAt transaction.tdate posting.pamount))
        | transaction <- snapshot.transactions
        , transaction.tdate <= day
        , any (isInvestmentAccount assetClasses scope . (.paccount)) transaction.tpostings
        , mapping <- selectedMappings
        , any
            ( \performance ->
                incomeLike performance.role
                    && any
                        (\candidate -> performance.account `accountWithin` candidate.paccount)
                        transaction.tpostings
            )
            mapping.performanceAccounts
        , posting <- transaction.tpostings
        , isInvestmentAccount assetClasses scope posting.paccount
        , mappedAssetScope mapping `accountWithin` posting.paccount
        ]
    mappedAssetScope mapping = case scope of
        Just selected
            | mapping.assetAccount `accountWithin` selected -> selected
        _ -> mapping.assetAccount
    incomeLike role = case role of
        Investment.Interest -> True
        Investment.Dividend -> True
        Investment.OtherIncome -> True
        Investment.RealisedGain -> False
        Investment.Fee -> False

{- | External investor cash flows for an investment scope, valued on each
transaction date. Contributions are negative; withdrawals and cash-paid income
are positive. The calculation uses postings outside the selected investment
scope and excludes mapped performance postings. This preserves actual purchase
and sale cash, while reinvested income, deducted fees, and transfers wholly
inside the scope cancel to zero.
-}
externalCashFlowsAt ::
    Snapshot ->
    Investment.AssetClassMappings ->
    Investment.InvestmentMappings ->
    Text ->
    Maybe Text ->
    Day ->
    Maybe [Xirr.CashFlow]
externalCashFlowsAt snapshot assetClasses mappings commodity scope day =
    externalCashFlowsAtWith
        (valueMixedAt snapshot commodity)
        snapshot
        assetClasses
        mappings
        commodity
        scope
        day

externalCashFlowsAtWith ::
    (Day -> Hledger.MixedAmount -> Hledger.MixedAmount) ->
    Snapshot ->
    Investment.AssetClassMappings ->
    Investment.InvestmentMappings ->
    Text ->
    Maybe Text ->
    Day ->
    Maybe [Xirr.CashFlow]
externalCashFlowsAtWith valueAt snapshot assetClasses mappings commodity scope day =
    traverse transactionCashFlow relevantTransactions
  where
    selectedMappings =
        filter
            (\mapping -> Investment.assetClassScopeOverlaps mapping.assetAccount assetClasses)
            ( case scope of
                Nothing -> Investment.mappingsForScope "" mappings
                Just selected -> Investment.mappingsForScope selected mappings
            )
    relevantTransactions =
        [ transaction
        | transaction <- snapshot.transactions
        , transaction.tdate <= day
        , transactionTouchesScope transaction || transactionHasSelectedPerformance transaction
        ]
    transactionTouchesScope =
        any (isInvestmentAccount assetClasses scope . (.paccount)) . (.tpostings)
    transactionHasSelectedPerformance transaction =
        any
            ( \mapping ->
                transactionMatchesScope scope mapping transaction
                    && any
                        ( \performance ->
                            any
                                (\posting -> performance.account `accountWithin` posting.paccount)
                                transaction.tpostings
                        )
                        mapping.performanceAccounts
            )
            selectedMappings
    transactionCashFlow transaction = do
        externalMovement <-
            valuePostings
                transaction
                [ posting
                | posting <- transaction.tpostings
                , not (isInvestmentAccount assetClasses scope posting.paccount)
                , not (isSelectedPerformancePosting transaction posting)
                ]
        pure (Xirr.cashFlow transaction.tdate externalMovement)
    valuePostings transaction =
        fmap sum
            . traverse
                (completeDecimal commodity . valueAt transaction.tdate . (.pamount))
    isSelectedPerformancePosting transaction posting =
        any
            ( \mapping ->
                transactionMatchesScope scope mapping transaction
                    && any
                        (\performance -> performance.account `accountWithin` posting.paccount)
                        mapping.performanceAccounts
            )
            selectedMappings

{- | 'summaryFromHoldings' passes cash flows already computed by 'scopeScansAt'
here, rather than this function re-scanning the journal for them.
-}
xirrFromCashFlows ::
    Maybe [Xirr.CashFlow] -> Day -> Maybe Decimal -> Maybe (Either Xirr.XirrError Decimal)
xirrFromCashFlows flows day terminalMarket = do
    cashFlows <- flows
    terminal <- terminalMarket
    pure (Xirr.annualRate <$> Xirr.xirr (cashFlows <> [Xirr.cashFlow day terminal]))

{- | Whether an 'Xirr.XirrError' represents a genuine ambiguity or failure to
converge, as opposed to the ordinary "not enough data" cases that should stay
silent.
-}
isNonTrivialXirrError :: Xirr.XirrError -> Bool
isNonTrivialXirrError err = case err of
    Xirr.InsufficientCashFlows -> False
    Xirr.CashFlowsDoNotChangeSign -> False
    Xirr.CashFlowOutsideNumericalRange -> True
    Xirr.NoConvergentRate -> True
    Xirr.AmbiguousRates -> True

transactionMatchesScope :: Maybe Text -> Investment.InvestmentMapping -> Hledger.Transaction -> Bool
transactionMatchesScope scope mapping transaction = case scope of
    Just selected
        | mapping.assetAccount /= selected
            && mapping.assetAccount `accountWithin` selected ->
            any
                (\candidate -> selected `accountWithin` candidate.paccount)
                transaction.tpostings
    _ -> True

hasAttributablePerformance ::
    Snapshot ->
    Investment.InvestmentMappings ->
    Day ->
    Text ->
    Bool
hasAttributablePerformance snapshot mappings day asset =
    case Investment.mappingForAccount asset mappings of
        Just _ -> True
        Nothing -> accountUsesExplicitCost snapshot day asset

accountUsesExplicitCost :: Snapshot -> Day -> Text -> Bool
accountUsesExplicitCost snapshot day asset =
    any
        (\amount -> abs amount.aquantity >= 0.000001 && amount.acost /= Nothing)
        [ amount
        | transaction <- snapshot.transactions
        , transaction.tdate <= day
        , posting <- transaction.tpostings
        , posting.paccount == asset
        , amount <- HledgerAmount.amountsRaw posting.pamount
        ]

-- | The journal transactions that touch an investment account within scope.
investmentDataTransactions ::
    Snapshot -> Investment.AssetClassMappings -> Maybe Text -> [Hledger.Transaction]
investmentDataTransactions snapshot assetClasses scope =
    filter
        (any (isInvestmentAccount assetClasses scope . (.paccount)) . (.tpostings))
        snapshot.transactions

investmentAccountsAt :: Snapshot -> Investment.AssetClassMappings -> Maybe Text -> Day -> [Text]
investmentAccountsAt snapshot assetClasses scope day =
    Set.toList . Set.fromList $
        [ posting.paccount
        | transaction <- snapshot.transactions
        , transaction.tdate <= day
        , posting <- transaction.tpostings
        , isInvestmentAccount assetClasses scope posting.paccount
        ]

isInvestmentAccount :: Investment.AssetClassMappings -> Maybe Text -> Text -> Bool
isInvestmentAccount assetClasses scope name =
    Investment.isAssetClassAccount name assetClasses
        && case scope of
            Just selected -> selected `accountWithin` name
            Nothing -> True

immediateChildren :: Snapshot -> Investment.AssetClassMappings -> Maybe Text -> [Text]
immediateChildren snapshot assetClasses scope =
    childrenOf
        scope
        (investmentAccountsAt snapshot assetClasses scope (fromMaybe epoch snapshot.latestJournalDate))

{- | The account list is a parameter, not rediscovered here, so a single
transactions x postings scan can serve both a parent's children and every
child's own child-check.
-}
childrenOf :: Maybe Text -> [Text] -> [Text]
childrenOf scope accounts =
    List.sort . Set.toList . Set.fromList $
        mapMaybe childOf (filter (\name -> maybe True (`accountWithin` name) scope) accounts)
  where
    parentParts = maybe ["assets"] HledgerAccountName.accountNameComponents scope
    childOf name =
        let parts = HledgerAccountName.accountNameComponents name
         in if length parts > length parentParts
                then Just (Text.intercalate ":" (take (length parentParts + 1) parts))
                else Nothing

reportPeriods :: Granularity -> [Hledger.Transaction] -> Day -> [(Text, Day, Day)]
reportPeriods granularity transactions latest =
    [ (period.key, period.start, min reportLastDay period.end)
    | period <- selectedPeriods
    ]
  where
    first = fromMaybe latest (minimumMaybe (fmap (.tdate) transactions))
    reportPeriod = Hledger.PeriodBetween first (addDays 1 latest)
    dateSpan = HledgerPeriod.periodAsDateSpan reportPeriod
    periods = periodsFromDateSpan granularity dateSpan
    selectedPeriods = case granularity of
        Monthly -> takeLast 24 periods
        Yearly -> periods
    journal = HledgerJournal.nulljournal{Hledger.jtxns = transactions}
    reportSpec =
        HledgerReportOptions.defreportspec
            { HledgerReportOptions._rsDay = latest
            , HledgerReportOptions._rsReportOpts =
                HledgerReportOptions.defreportopts{HledgerReportOptions.period_ = reportPeriod}
            }
    reportLastDay =
        fromMaybe latest (HledgerReportOptions.reportPeriodOrJournalLastDay reportSpec journal)

validateScope :: Investment.InvestmentMappings -> Maybe Text -> Either AppError (Maybe Text)
validateScope _ requested = case requested >>= scopeText of
    Nothing -> Right Nothing
    Just scope
        | "assets" `accountWithin` scope ->
            Right (Just scope)
        | otherwise ->
            Left
                (InvalidRequest "Account prefix must be an asset account.")

scopeText :: Text -> Maybe Text
scopeText value
    | Text.null (Text.strip value) = Nothing
    | otherwise = Just (Text.strip value)

emptySummary :: InvestmentSummary
emptySummary =
    InvestmentSummary
        { marketValue = Nothing
        , purchaseCost = Nothing
        , gainLoss = Nothing
        , xirr = Nothing
        , attributedIncome = Nothing
        , realisedGain = Nothing
        , totalProfit = Nothing
        , completeness = Complete
        }
