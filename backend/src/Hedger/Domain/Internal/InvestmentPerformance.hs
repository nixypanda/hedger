{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Hedger.Domain.Internal.InvestmentPerformance (
    -- * Value objects
    InvestmentCompleteness (..),
    InvestmentSummary,
    Completeness,
    SummaryResult,
    summaryMarketValue,
    summaryPurchaseCost,
    summaryGainLoss,
    summaryXirr,
    summaryAttributedIncome,
    summaryRealisedGain,
    summaryTotalProfit,
    summaryWarnings,
    summaryCompleteness,
    InvestmentWarning (..),
    WarningKind (..),
    InvestmentPerformanceContext (..),
    Attribution (..),

    -- * Performance calculation
    prepareInvestmentPerformance,
    summaryAtPrepared,
    summariesAtPrepared,
    summariesAtScopesPrepared,
    summaryAt,
    investmentSummaryAt,

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
    scopeScansForScopesAt,
) where

import Data.Decimal (Decimal)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
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
    epoch,
    intervalFor,
    maximumMaybe,
    minimumMaybe,
    periodsFromDateSpan,
    reportPeriodEnd,
    reportPeriodKey,
    reportPeriodStart,
    takeLast,
 )
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (completeDecimal, missingCommodities, valueMixedAt)
import Hedger.Domain.Xirr qualified as Xirr

-- | The text form of a mapping's asset account, for prefix comparisons.
assetAccountTextOf :: Investment.InvestmentMapping -> Text
assetAccountTextOf mapping = Investment.assetAccountText mapping.assetAccount

-- | The text form of a mapped performance account, for prefix comparisons.
performanceAccountText :: Investment.PerformanceAccount -> Text
performanceAccountText performance = Investment.accountNameText performance.account

-- | The text form of a configured asset-class account prefix.
assetPrefixText :: Investment.AssetAccount -> Text
assetPrefixText = Investment.assetAccountText

-- | Whether a posting's account lies within a mapped performance account.
postingWithinPerformance :: Investment.PerformanceAccount -> Hledger.Posting -> Bool
postingWithinPerformance performance posting =
    performanceAccountText performance `accountWithin` posting.paccount

-- | Whether a mapping's asset account lies inside the configured asset-class scopes.
mappingScopeOverlaps ::
    Investment.InvestmentMapping -> Investment.AssetClassMappings -> Bool
mappingScopeOverlaps mapping assetClasses =
    Investment.assetClassScopeOverlaps (assetAccountTextOf mapping) assetClasses

data InvestmentCompleteness
    = Complete
    | Partial
    | Unavailable
    deriving stock (Eq, Show)

{- | The three attribution quantities carried per scope: income, realised gain
and fees, each already valued in the display commodity.
-}
data Attribution = Attribution
    { income :: Decimal
    , realisedGain :: Decimal
    , fees :: Decimal
    }
    deriving stock (Eq, Show)

-- | The completeness index of an 'InvestmentSummary'.
data Completeness
    = SummaryNoHoldingsK
    | SummaryUnavailableK
    | SummaryPartialK
    | SummaryCompleteK
    deriving stock (Eq, Show)

{- | An investment summary whose constructors encode how complete its evidence
is. 'SummaryComplete' carries no warnings; 'SummaryUnavailable' carries neither a
market value nor a purchase cost; each partial constructor either raised a
warning or withholds the total profit, so the completeness invariants are facts
of the type rather than runtime checks.
-}
data InvestmentSummary (c :: Completeness) where
    SummaryNoHoldings :: InvestmentSummary 'SummaryNoHoldingsK
    SummaryUnavailable ::
        Maybe Decimal -> -- attributed income
        Maybe Decimal -> -- realised gain
        [InvestmentWarning] ->
        InvestmentSummary 'SummaryUnavailableK
    SummaryPartialUnpriced ::
        UnpricedPartial ->
        [InvestmentWarning] ->
        InvestmentSummary 'SummaryPartialK
    SummaryPartialPricedWarned ::
        PricedPartial ->
        NonEmpty InvestmentWarning ->
        InvestmentSummary 'SummaryPartialK
    SummaryPartialPricedIncomplete ::
        PricedPartialNoTotal ->
        InvestmentSummary 'SummaryPartialK
    SummaryComplete ::
        CompleteSummary ->
        InvestmentSummary 'SummaryCompleteK

data UnpricedPartial = UnpricedPartial
    { purchaseCost :: Maybe Decimal
    , attributedIncome :: Maybe Decimal
    , realisedGain :: Maybe Decimal
    }

data PricedPartial = PricedPartial
    { marketValue :: Decimal
    , purchaseCost :: Maybe Decimal
    , gainLoss :: Maybe Decimal
    , xirr :: Maybe Decimal
    , attributedIncome :: Maybe Decimal
    , realisedGain :: Maybe Decimal
    , totalProfit :: Maybe Decimal
    }

data PricedPartialNoTotal = PricedPartialNoTotal
    { marketValue :: Decimal
    , purchaseCost :: Maybe Decimal
    , gainLoss :: Maybe Decimal
    , xirr :: Maybe Decimal
    , attributedIncome :: Maybe Decimal
    , realisedGain :: Maybe Decimal
    }

data CompleteSummary = CompleteSummary
    { marketValue :: Decimal
    , purchaseCost :: Maybe Decimal
    , gainLoss :: Decimal
    , xirr :: Maybe Decimal
    , attributedIncome :: Decimal
    , realisedGain :: Decimal
    , totalProfit :: Decimal
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

{- | An existentially packed summary, so the various entry points can return one
regardless of the completeness index.
-}
data SummaryResult = forall c. SummaryResult (InvestmentSummary c)

marketValue :: InvestmentSummary c -> Maybe Decimal
marketValue summary =
    case summary of
        SummaryNoHoldings -> Nothing
        SummaryUnavailable{} -> Nothing
        SummaryPartialUnpriced{} -> Nothing
        SummaryPartialPricedWarned priced _ -> Just priced.marketValue
        SummaryPartialPricedIncomplete priced -> Just priced.marketValue
        SummaryComplete complete -> Just complete.marketValue

purchaseCost :: InvestmentSummary c -> Maybe Decimal
purchaseCost summary =
    case summary of
        SummaryNoHoldings -> Nothing
        SummaryUnavailable{} -> Nothing
        SummaryPartialUnpriced unpriced _ -> unpriced.purchaseCost
        SummaryPartialPricedWarned priced _ -> priced.purchaseCost
        SummaryPartialPricedIncomplete priced -> priced.purchaseCost
        SummaryComplete complete -> complete.purchaseCost

gainLoss :: InvestmentSummary c -> Maybe Decimal
gainLoss summary =
    case summary of
        SummaryNoHoldings -> Nothing
        SummaryUnavailable{} -> Nothing
        SummaryPartialUnpriced{} -> Nothing
        SummaryPartialPricedWarned priced _ -> priced.gainLoss
        SummaryPartialPricedIncomplete priced -> priced.gainLoss
        SummaryComplete complete -> Just complete.gainLoss

xirr :: InvestmentSummary c -> Maybe Decimal
xirr summary =
    case summary of
        SummaryNoHoldings -> Nothing
        SummaryUnavailable{} -> Nothing
        SummaryPartialUnpriced{} -> Nothing
        SummaryPartialPricedWarned priced _ -> priced.xirr
        SummaryPartialPricedIncomplete priced -> priced.xirr
        SummaryComplete complete -> complete.xirr

attributedIncome :: InvestmentSummary c -> Maybe Decimal
attributedIncome summary =
    case summary of
        SummaryNoHoldings -> Nothing
        SummaryUnavailable income _ _ -> income
        SummaryPartialUnpriced unpriced _ -> unpriced.attributedIncome
        SummaryPartialPricedWarned priced _ -> priced.attributedIncome
        SummaryPartialPricedIncomplete priced -> priced.attributedIncome
        SummaryComplete complete -> Just complete.attributedIncome

realisedGain :: InvestmentSummary c -> Maybe Decimal
realisedGain summary =
    case summary of
        SummaryNoHoldings -> Nothing
        SummaryUnavailable _ realised _ -> realised
        SummaryPartialUnpriced unpriced _ -> unpriced.realisedGain
        SummaryPartialPricedWarned priced _ -> priced.realisedGain
        SummaryPartialPricedIncomplete priced -> priced.realisedGain
        SummaryComplete complete -> Just complete.realisedGain

totalProfit :: InvestmentSummary c -> Maybe Decimal
totalProfit summary =
    case summary of
        SummaryNoHoldings -> Nothing
        SummaryUnavailable{} -> Nothing
        SummaryPartialUnpriced{} -> Nothing
        SummaryPartialPricedWarned priced _ -> priced.totalProfit
        SummaryPartialPricedIncomplete{} -> Nothing
        SummaryComplete complete -> Just complete.totalProfit

warnings :: InvestmentSummary c -> [InvestmentWarning]
warnings summary =
    case summary of
        SummaryNoHoldings -> []
        SummaryUnavailable _ _ raised -> raised
        SummaryPartialUnpriced _ raised -> raised
        SummaryPartialPricedWarned _ raised -> NonEmpty.toList raised
        SummaryPartialPricedIncomplete{} -> []
        SummaryComplete{} -> []

completeness :: InvestmentSummary c -> InvestmentCompleteness
completeness summary =
    case summary of
        SummaryNoHoldings -> Complete
        SummaryUnavailable{} -> Unavailable
        SummaryPartialUnpriced{} -> Partial
        SummaryPartialPricedWarned{} -> Partial
        SummaryPartialPricedIncomplete{} -> Partial
        SummaryComplete{} -> Complete

summaryMarketValue :: SummaryResult -> Maybe Decimal
summaryMarketValue (SummaryResult summary) = marketValue summary

summaryPurchaseCost :: SummaryResult -> Maybe Decimal
summaryPurchaseCost (SummaryResult summary) = purchaseCost summary

summaryGainLoss :: SummaryResult -> Maybe Decimal
summaryGainLoss (SummaryResult summary) = gainLoss summary

summaryXirr :: SummaryResult -> Maybe Decimal
summaryXirr (SummaryResult summary) = xirr summary

summaryAttributedIncome :: SummaryResult -> Maybe Decimal
summaryAttributedIncome (SummaryResult summary) = attributedIncome summary

summaryRealisedGain :: SummaryResult -> Maybe Decimal
summaryRealisedGain (SummaryResult summary) = realisedGain summary

summaryTotalProfit :: SummaryResult -> Maybe Decimal
summaryTotalProfit (SummaryResult summary) = totalProfit summary

summaryWarnings :: SummaryResult -> [InvestmentWarning]
summaryWarnings (SummaryResult summary) = warnings summary

summaryCompleteness :: SummaryResult -> InvestmentCompleteness
summaryCompleteness (SummaryResult summary) = completeness summary

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
account discovery, mapping completeness, and the performance quantities
(retained income, attribution, and cash flows) that each require a full prefix
scan of the journal. 'scopeScansAt' computes all of them in one chronological
sweep instead of one scan per quantity per day.
-}
data ScopeScan = ScopeScan
    { accounts :: [Text]
    , mappingComplete :: Bool
    , retained :: Maybe Decimal
    , attributed :: Maybe Attribution
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
        HledgerValuation.mixedAmountValueAtDate priceOracle commodityStyles (Just commodity) day
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
        ( holdingsSeriesAtWithJournal
            context.snapshot.journal
            context.assetClasses
            scope
            granularity
            days
        )
        ( holdingsSeriesAtWithJournal
            context.historicalCostJournal
            context.assetClasses
            scope
            granularity
            days
        )
        (scopeScansAt context scope days)

{- | Every latest-date summary needed for drill-down and allocation views.
Holdings come from one flat market report and one flat historical-cost report,
while the non-holdings inputs are accumulated for every scope in one traversal.
Results preserve the requested scope order, including duplicates.
-}
summariesAtScopesPrepared ::
    InvestmentPerformanceContext -> Day -> [Maybe Text] -> [(Maybe Text, SummaryResult)]
summariesAtScopesPrepared context day scopes =
    zip
        scopes
        ( List.zipWith4
            (\scope -> summaryFromHoldings context scope day)
            scopes
            ( holdingsForScopesAtWithJournal
                context.snapshot.journal
                context.assetClasses
                scopes
                day
            )
            holdingsForCost
            (scopeScansForScopesAt context scopes day)
        )
  where
    holdingsForCost =
        holdingsForScopesAtWithJournal context.historicalCostJournal context.assetClasses scopes day

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
    summary
  where
    commodity = context.commodity
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
    income = (.income) <$> attributed
    realised = (.realisedGain) <$> attributed
    fees = (.fees) <$> attributed
    total = (+) <$> calculatedGain <*> ((+) <$> ((+) <$> income <*> realised) <*> fees)
    xirrResult =
        if mappingComplete
            then xirrFromCashFlows scopeScan.cashFlows day market
            else Nothing
    annualisedReturn = xirrResult >>= either (const Nothing) Just
    unresolvedReturn = maybe False (either isNonTrivialXirrError (const False)) xirrResult
    missingValue = missingCommodities commodity valueRaw
    missingCost = missingCommodities commodity costValued <> rawMissingCost
    raisedWarnings =
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
            <> [InvestmentWarning UnresolvedReturn scope | unresolvedReturn]
    summary
        | not hasHoldings = SummaryResult SummaryNoHoldings
        | market == Nothing && cost == Nothing =
            SummaryResult (SummaryUnavailable income realised raisedWarnings)
        | otherwise =
            case market of
                Just marketValue' ->
                    if null raisedWarnings
                        then case (calculatedGain, income, realised, total) of
                            ( Just gainValue
                                , Just incomeValue
                                , Just realisedValue
                                , Just totalValue
                                ) ->
                                    SummaryResult
                                        ( SummaryComplete
                                            CompleteSummary
                                                { marketValue = marketValue'
                                                , purchaseCost = cost
                                                , gainLoss = gainValue
                                                , xirr = annualisedReturn
                                                , attributedIncome = incomeValue
                                                , realisedGain = realisedValue
                                                , totalProfit = totalValue
                                                }
                                        )
                            _ ->
                                SummaryResult
                                    ( SummaryPartialPricedIncomplete
                                        PricedPartialNoTotal
                                            { marketValue = marketValue'
                                            , purchaseCost = cost
                                            , gainLoss = calculatedGain
                                            , xirr = annualisedReturn
                                            , attributedIncome = income
                                            , realisedGain = realised
                                            }
                                    )
                        else
                            SummaryResult
                                ( SummaryPartialPricedWarned
                                    PricedPartial
                                        { marketValue = marketValue'
                                        , purchaseCost = cost
                                        , gainLoss = calculatedGain
                                        , xirr = annualisedReturn
                                        , attributedIncome = income
                                        , realisedGain = realised
                                        , totalProfit = total
                                        }
                                    (NonEmpty.fromList raisedWarnings)
                                )
                Nothing ->
                    SummaryResult
                        ( SummaryPartialUnpriced
                            UnpricedPartial
                                { purchaseCost = cost
                                , attributedIncome = income
                                , realisedGain = realised
                                }
                            raisedWarnings
                        )

-- | Running totals threaded through 'scopeScansAt''s chronological sweep.
data Accumulator = Accumulator
    { seenAccounts :: !(Set.Set Text)
    , explicitCostAccounts :: !(Set.Set Text)
    , retainedTotal :: !(Maybe Decimal)
    , attributedTotal :: !(Maybe Attribution)
    , -- Newest first: 'step' conses, and 'snapshotAt' reverses once per emitted day.
      cashFlowsTotal :: !(Maybe [Xirr.CashFlow])
    }

initialAccumulator :: Accumulator
initialAccumulator =
    Accumulator
        { seenAccounts = Set.empty
        , explicitCostAccounts = Set.empty
        , retainedTotal = Just 0
        , attributedTotal = Just (Attribution 0 0 0)
        , cashFlowsTotal = Just []
        }

data ScopePlan = ScopePlan
    { selectedScope :: !(Maybe Text)
    , selectedMappings :: ![Investment.InvestmentMapping]
    }

scopePlan :: InvestmentPerformanceContext -> Maybe Text -> ScopePlan
scopePlan context selectedScope =
    ScopePlan
        { selectedScope
        , selectedMappings =
            filter
                ( \mapping ->
                    Investment.assetClassScopeOverlaps
                        (assetAccountTextOf mapping)
                        context.assetClasses
                )
                ( case selectedScope of
                    Nothing -> Investment.mappingsForScope "" context.mappings
                    Just selected -> Investment.mappingsForScope selected context.mappings
                )
        }

isIncomeLike :: Investment.PerformanceRole -> Bool
isIncomeLike role =
    case role of
        Investment.Interest -> True
        Investment.Dividend -> True
        Investment.OtherIncome -> True
        Investment.RealisedGain -> False
        Investment.Fee -> False

stepAccumulator ::
    InvestmentPerformanceContext -> ScopePlan -> Accumulator -> Hledger.Transaction -> Accumulator
stepAccumulator context plan acc transaction =
    if
        | not (touchesPortfolio || transactionHasSelectedPerformance) -> acc
        | otherwise ->
            acc
                { seenAccounts = List.foldl' addAccount acc.seenAccounts transaction.tpostings
                , explicitCostAccounts =
                    List.foldl' addExplicitCost acc.explicitCostAccounts transaction.tpostings
                , retainedTotal = List.foldl' addSticky acc.retainedTotal retainedContributions
                , attributedTotal =
                    List.foldl' combineAttribution acc.attributedTotal attributedContributions
                , cashFlowsTotal = addCashFlow acc.cashFlowsTotal transaction
                }
  where
    scope = plan.selectedScope
    commodity = context.commodity
    valueAt = context.valueAt
    mappedAssetScope mapping =
        case scope of
            Just selected
                | assetAccountTextOf mapping `accountWithin` selected -> selected
            _ -> assetAccountTextOf mapping
    touchesPortfolio =
        any (isInvestmentAccount context.assetClasses scope . (.paccount)) transaction.tpostings
    transactionHasSelectedPerformance =
        any
            ( \mapping ->
                transactionMatchesScope scope mapping transaction
                    && any
                        ( \performance ->
                            any
                                (postingWithinPerformance performance)
                                transaction.tpostings
                        )
                        mapping.performanceAccounts
            )
            plan.selectedMappings
    isSelectedPerformancePosting posting =
        any
            ( \mapping ->
                transactionMatchesScope scope mapping transaction
                    && any
                        (\performance -> postingWithinPerformance performance posting)
                        mapping.performanceAccounts
            )
            plan.selectedMappings
    valuePostings =
        fmap sum . traverse (completeDecimal commodity . valueAt transaction.tdate . (.pamount))
    transactionCashFlow = do
        externalMovement <-
            valuePostings
                [ posting
                | posting <- transaction.tpostings
                , not (isInvestmentAccount context.assetClasses scope posting.paccount)
                , not (isSelectedPerformancePosting posting)
                ]
        pure (Xirr.cashFlow transaction.tdate externalMovement)
    retainedContributions
        | not touchesPortfolio = []
        | otherwise =
            [ fmap (max 0) (completeDecimal commodity (valueAt transaction.tdate posting.pamount))
            | mapping <- plan.selectedMappings
            , any
                ( \performance ->
                    isIncomeLike performance.role
                        && any
                            (\candidate -> postingWithinPerformance performance candidate)
                            transaction.tpostings
                )
                mapping.performanceAccounts
            , posting <- transaction.tpostings
            , isInvestmentAccount context.assetClasses scope posting.paccount
            , mappedAssetScope mapping `accountWithin` posting.paccount
            ]
    attributedContributions
        | not touchesPortfolio = []
        | otherwise =
            [ do
                quantity <- completeDecimal commodity (valueAt transaction.tdate posting.pamount)
                pure $
                    case performance.role of
                        Investment.RealisedGain ->
                            Attribution
                                { income = 0
                                , realisedGain = negate quantity
                                , fees = 0
                                }
                        Investment.Fee ->
                            Attribution
                                { income = 0
                                , realisedGain = 0
                                , fees = negate quantity
                                }
                        _ ->
                            Attribution
                                { income = negate quantity
                                , realisedGain = 0
                                , fees = 0
                                }
            | mapping <- plan.selectedMappings
            , transactionMatchesScope scope mapping transaction
            , posting <- transaction.tpostings
            , performance <- mapping.performanceAccounts
            , performanceAccountText performance `accountWithin` posting.paccount
            ]
    hasExplicitCost amount = abs amount.aquantity >= 0.000001 && amount.acost /= Nothing
    addSticky Nothing _ = Nothing
    addSticky _ Nothing = Nothing
    addSticky (Just total) (Just value) = Just (total + value)
    addAccount accounts posting
        | isInvestmentAccount context.assetClasses scope posting.paccount =
            Set.insert posting.paccount accounts
        | otherwise = accounts
    addExplicitCost accounts posting
        | any hasExplicitCost (HledgerAmount.amountsRaw posting.pamount) =
            Set.insert posting.paccount accounts
        | otherwise = accounts
    addCashFlow Nothing _ = Nothing
    addCashFlow (Just flows) _
        | not (touchesPortfolio || transactionHasSelectedPerformance) = Just flows
        | otherwise =
            case transactionCashFlow of
                Nothing -> Nothing
                Just flow -> Just (flow : flows)

snapshotAccumulator :: InvestmentPerformanceContext -> Accumulator -> ScopeScan
snapshotAccumulator context acc =
    ScopeScan
        { accounts = Set.toList acc.seenAccounts
        , mappingComplete =
            not (Set.null acc.seenAccounts)
                && all
                    ( \account ->
                        isJust (Investment.mappingForAccount account context.mappings)
                            || Set.member account acc.explicitCostAccounts
                    )
                    (Set.toList acc.seenAccounts)
        , retained = acc.retainedTotal
        , attributed = acc.attributedTotal
        , cashFlows = fmap reverse acc.cashFlowsTotal
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
    plan = scopePlan context scope
    indexedDays = List.sortOn snd (zip [0 :: Int ..] days)
    indexedResults = walk initialAccumulator context.sortedTransactions indexedDays
    walk :: Accumulator -> [Hledger.Transaction] -> [(Int, Day)] -> [(Int, ScopeScan)]
    walk _ _ [] = []
    walk acc [] pending = [(index, snapshotAccumulator context acc) | (index, _) <- pending]
    walk acc allTransactions@(transaction : restTransactions) pending@((index, day) : restPending)
        | transaction.tdate <= day =
            walk (stepAccumulator context plan acc transaction) restTransactions pending
        | otherwise =
            (index, snapshotAccumulator context acc)
                : walk acc allTransactions restPending

{- | The latest-date non-holdings inputs for several scopes, produced by one
traversal. Scope-specific predicates remain independent because transfers and
performance mappings are not additive across account prefixes.
-}
scopeScansForScopesAt :: InvestmentPerformanceContext -> [Maybe Text] -> Day -> [ScopeScan]
scopeScansForScopesAt _ [] _ = []
scopeScansForScopesAt context scopes day = fmap (snapshotAccumulator context) finalAccumulators
  where
    plans = fmap (scopePlan context) scopes
    transactions = takeWhile ((<= day) . (.tdate)) context.sortedTransactions
    finalAccumulators =
        List.foldl' stepAll (replicate (length plans) initialAccumulator) transactions
    stepAll accumulators transaction =
        List.zipWith (\plan acc -> stepAccumulator context plan acc transaction) plans accumulators

investmentSummaryAt ::
    Snapshot ->
    Investment.AssetClassMappings ->
    Investment.InvestmentMappings ->
    Text ->
    Maybe Text ->
    Day ->
    SummaryResult
investmentSummaryAt snapshot assetClasses mappings commodity scope day =
    summaryAt snapshot assetClasses mappings commodity scope day

holdingsAt :: Snapshot -> Investment.AssetClassMappings -> Maybe Text -> Day -> Hledger.MixedAmount
holdingsAt snapshot = holdingsAtWithJournal snapshot.journal

holdingsAtWithJournal ::
    Hledger.Journal -> Investment.AssetClassMappings -> Maybe Text -> Day -> Hledger.MixedAmount
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

{- | Holdings for several scopes projected from one flat balance report. The
first account name in a 'BalanceReportItem' is the full account name; the
second is only its display name and cannot safely be used for prefix matching.
-}
holdingsForScopesAtWithJournal ::
    Hledger.Journal ->
    Investment.AssetClassMappings ->
    [Maybe Text] ->
    Day ->
    [Hledger.MixedAmount]
holdingsForScopesAtWithJournal _ _ [] _ = []
holdingsForScopesAtWithJournal journal assetClasses scopes day = fmap holdingsFor scopes
  where
    (rows, _) = HledgerBalance.balanceReport (spec Nothing) journal
    holdingsFor scope =
        mconcat
            [ amount
            | (account, _, _, amount) <- rows
            , isInvestmentAccount assetClasses scope account
            ]
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
            let periodStart =
                    HledgerDayPartition.intervalBoundaryBefore (intervalFor granularity) earliest
                period = Hledger.PeriodBetween periodStart (addDays 1 latest)
                options =
                    HledgerReportOptions.defreportopts
                        { HledgerReportOptions.period_ = period
                        , HledgerReportOptions.interval_ = intervalFor granularity
                        , HledgerReportOptions.balanceaccum_ = HledgerReportOptions.Historical
                        , HledgerReportOptions.accountlistmode_ = HledgerReportOptions.ALFlat
                        , HledgerReportOptions.empty_ = True
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
                containsDay day (columnSpan, _) = HledgerDates.spanContainsDate columnSpan day
                amountFor day =
                    case List.find (containsDay day) columns of
                        Just (_, amount) -> amount
                        Nothing -> mempty
             in fmap amountFor days
        -- Without this, hledger drops columns for periods with no activity,
        -- and the series would lose entries for those requested days.
        -- Requested days don't correspond positionally to report columns (a clamped
        -- final period, or a day predating any data), so look up by span membership.
        _ -> []

journalWithHistoricalCosts :: Snapshot -> Text -> Hledger.Journal
journalWithHistoricalCosts snapshot commodity =
    journalWithHistoricalCostsUsing (valueMixedAt snapshot commodity) snapshot commodity

journalWithHistoricalCostsUsing ::
    (Day -> Hledger.MixedAmount -> Hledger.MixedAmount) -> Snapshot -> Text -> Hledger.Journal
journalWithHistoricalCostsUsing valueAt snapshot commodity =
    snapshot.journal{Hledger.jtxns = convertTransaction <$> snapshot.journal.jtxns}
  where
    convertTransaction transaction =
        transaction{Hledger.tpostings = convertPosting transaction.tdate <$> transaction.tpostings}
    convertPosting day posting =
        posting
            { Hledger.pamount =
                HledgerAmount.mapMixedAmount (convertAmount day posting.paccount) posting.pamount
            }
    convertAmount day account amount =
        amount
            { Hledger.acost =
                case amount.acost of
                    Just acquisitionCost -> Just (convertCost day acquisitionCost)
                    Nothing -> inferredCashCost day account amount
            }
    convertCost day acquisitionCost =
        case acquisitionCost of
            Hledger.UnitCost amount -> Hledger.UnitCost (convertCostAmount day amount)
            Hledger.TotalCost amount -> Hledger.TotalCost (convertCostAmount day amount)
    convertCostAmount day amount =
        fromMaybe
            amount
            (HledgerAmount.unifyMixedAmount (valueAt day (HledgerAmount.mixedAmount amount)))
    inferredCashCost day account amount
        | "assets:cash" `accountWithin` account && amount.acommodity /= commodity =
            Hledger.UnitCost <$> historicalUnitPrice day amount
        | otherwise = Nothing
    historicalUnitPrice day amount = do
        valued <-
            HledgerAmount.unifyMixedAmount
                ( valueAt
                    day
                    ( HledgerAmount.mixedAmount
                        amount{Hledger.aquantity = 1, Hledger.acost = Nothing}
                    )
                )
        if valued.acommodity == commodity
            then Just valued
            else Nothing

investmentQuery :: Investment.AssetClassMappings -> Maybe Text -> HledgerQuery.Query
investmentQuery assetClasses scope =
    HledgerQuery.And ([HledgerQuery.Type [Hledger.Asset], configuredAccounts] <> scopeQuery)
  where
    configuredAccounts =
        case configuredQueries of
            [] -> HledgerQuery.None
            queries -> HledgerQuery.Or queries
    configuredQueries =
        [ HledgerQuery.Acct
            (HledgerAccountName.accountNameToAccountRegex (assetPrefixText prefix))
        | mapping <- Investment.assetClassMappingsList assetClasses
        , prefix <- NonEmpty.toList mapping.accountPrefixes
        ]
    scopeQuery =
        maybe
            []
            (\name -> [HledgerQuery.Acct (HledgerAccountName.accountNameToAccountRegex name)])
            scope

-- | Add two attributions component-wise, propagating a missing side.
combineAttribution :: Maybe Attribution -> Maybe Attribution -> Maybe Attribution
combineAttribution Nothing _ = Nothing
combineAttribution _ Nothing = Nothing
combineAttribution (Just first) (Just second) =
    Just
        Attribution
            { income = first.income + second.income
            , realisedGain = first.realisedGain + second.realisedGain
            , fees = first.fees + second.fees
            }

attributedAt ::
    Snapshot ->
    Investment.AssetClassMappings ->
    Investment.InvestmentMappings ->
    Text ->
    Maybe Text ->
    Day ->
    Maybe Attribution
attributedAt snapshot assetClasses mappings commodity scope day =
    attributedAtWith
        (valueMixedAt snapshot commodity)
        snapshot
        assetClasses
        mappings
        commodity
        scope
        day

attributedAtWith ::
    (Day -> Hledger.MixedAmount -> Hledger.MixedAmount) ->
    Snapshot ->
    Investment.AssetClassMappings ->
    Investment.InvestmentMappings ->
    Text ->
    Maybe Text ->
    Day ->
    Maybe Attribution
attributedAtWith valueAt snapshot assetClasses mappings commodity scope day =
    foldl combineAttribution (Just (Attribution 0 0 0)) contributions
  where
    selectedMappings =
        filter
            (\mapping -> mappingScopeOverlaps mapping assetClasses)
            ( case scope of
                Nothing -> Investment.mappingsForScope "" mappings
                Just selected -> Investment.mappingsForScope selected mappings
            )
    contributions =
        [ do
            quantity <- completeDecimal commodity (valueAt transaction.tdate posting.pamount)
            pure $
                case performance.role of
                    Investment.RealisedGain ->
                        Attribution
                            { income = 0
                            , realisedGain = negate quantity
                            , fees = 0
                            }
                    Investment.Fee ->
                        Attribution
                            { income = 0
                            , realisedGain = 0
                            , fees = negate quantity
                            }
                    _ ->
                        Attribution
                            { income = negate quantity
                            , realisedGain = 0
                            , fees = 0
                            }
        | transaction <- snapshot.transactions
        , transaction.tdate <= day
        , transactionTouchesPortfolio transaction
        , mapping <- selectedMappings
        , transactionMatchesScope scope mapping transaction
        , posting <- transaction.tpostings
        , performance <- mapping.performanceAccounts
        , performanceAccountText performance `accountWithin` posting.paccount
        ]
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
            (\mapping -> mappingScopeOverlaps mapping assetClasses)
            ( case scope of
                Nothing -> Investment.mappingsForScope "" mappings
                Just selected -> Investment.mappingsForScope selected mappings
            )
    retainedAmounts =
        [ fmap (max 0) (completeDecimal commodity (valueAt transaction.tdate posting.pamount))
        | transaction <- snapshot.transactions
        , transaction.tdate <= day
        , any (isInvestmentAccount assetClasses scope . (.paccount)) transaction.tpostings
        , mapping <- selectedMappings
        , any
            ( \performance ->
                incomeLike performance.role
                    && any
                        (\candidate -> postingWithinPerformance performance candidate)
                        transaction.tpostings
            )
            mapping.performanceAccounts
        , posting <- transaction.tpostings
        , isInvestmentAccount assetClasses scope posting.paccount
        , mappedAssetScope mapping `accountWithin` posting.paccount
        ]
    mappedAssetScope mapping =
        case scope of
            Just selected
                | assetAccountTextOf mapping `accountWithin` selected -> selected
            _ -> assetAccountTextOf mapping
    incomeLike role =
        case role of
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
            (\mapping -> mappingScopeOverlaps mapping assetClasses)
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
                                (postingWithinPerformance performance)
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
        fmap sum . traverse (completeDecimal commodity . valueAt transaction.tdate . (.pamount))
    isSelectedPerformancePosting transaction posting =
        any
            ( \mapping ->
                transactionMatchesScope scope mapping transaction
                    && any
                        (\performance -> postingWithinPerformance performance posting)
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

{- | Whether an 'Xirr.XirrError' represents a genuine ambiguity or failure
to converge, as opposed to the ordinary "not enough data" cases that should
stay silent.
-}
isNonTrivialXirrError :: Xirr.XirrError -> Bool
isNonTrivialXirrError err =
    case err of
        Xirr.InsufficientCashFlows -> False
        Xirr.CashFlowsDoNotChangeSign -> False
        Xirr.CashFlowOutsideNumericalRange -> True
        Xirr.NoConvergentRate -> True
        Xirr.AmbiguousRates -> True

transactionMatchesScope :: Maybe Text -> Investment.InvestmentMapping -> Hledger.Transaction -> Bool
transactionMatchesScope scope mapping transaction =
    case scope of
        Just selected
            | assetAccountTextOf mapping /= selected
                && assetAccountTextOf mapping `accountWithin` selected ->
                any
                    ( \candidate ->
                        selected `accountWithin` candidate.paccount
                    )
                    transaction.tpostings
        _ -> True

hasAttributablePerformance :: Snapshot -> Investment.InvestmentMappings -> Day -> Text -> Bool
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

investmentAccountsAt ::
    Snapshot -> Investment.AssetClassMappings -> Maybe Text -> Day -> [Text]
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

immediateChildren ::
    Snapshot -> Investment.AssetClassMappings -> Maybe Text -> [Text]
immediateChildren snapshot assetClasses scope =
    childrenOf
        scope
        ( investmentAccountsAt
            snapshot
            assetClasses
            scope
            (fromMaybe epoch snapshot.latestJournalDate)
        )

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
    [ (reportPeriodKey period, reportPeriodStart period, min reportLastDay (reportPeriodEnd period))
    | period <- selectedPeriods
    ]
  where
    first = fromMaybe latest (minimumMaybe (fmap (.tdate) transactions))
    reportPeriod = Hledger.PeriodBetween first (addDays 1 latest)
    dateSpan = HledgerPeriod.periodAsDateSpan reportPeriod
    periods = periodsFromDateSpan granularity dateSpan
    selectedPeriods =
        case granularity of
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
validateScope _ requested =
    case requested >>= scopeText of
        Nothing -> Right Nothing
        Just scope
            | "assets" `accountWithin` scope -> Right (Just scope)
            | otherwise -> Left (InvalidRequest "Account prefix must be an asset account.")

scopeText :: Text -> Maybe Text
scopeText value
    | Text.null (Text.strip value) = Nothing
    | otherwise = Just (Text.strip value)
