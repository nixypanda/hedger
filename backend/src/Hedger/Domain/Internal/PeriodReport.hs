{- | Shared period-report kernel: the hledger balance/flow report plumbing and
the line-item value objects used by the net-worth and cashflow calculations.
-}
module Hedger.Domain.Internal.PeriodReport (
    TimelinePoint (..),
    BreakdownItem (..),
    ReportSide (..),
    periodicBalanceReport,
    periodicBalanceReportIn,
    periodicBalanceColumns,
    reportJournal,
    reportValuation,
    wholeReport,
    reportSide,
    reportWindow,
    reportPeriodsFrom,
    periodsWithData,
    reportTotals,
    reportMissing,
    selectedPeriod,
    breakdownItemsFor,
)
where

import Data.Decimal (Decimal)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (Day, addDays)

import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Journal qualified as HledgerJournal
import Hledger.Data.Period qualified as HledgerPeriod
import Hledger.Data.Types qualified as Hledger
import Hledger.Data.Valuation qualified as HledgerValuation
import Hledger.Query qualified as HledgerQuery
import Hledger.Reports.MultiBalanceReport qualified as HledgerMultiBalance
import Hledger.Reports.ReportOptions qualified as HledgerReportOptions
import Hledger.Reports.ReportTypes qualified as HledgerReportTypes

import Hedger.Domain.Account (atDepth)
import Hedger.Domain.Period (
    Granularity (..),
    ReportPeriod (..),
    atIndex,
    epoch,
    intervalFor,
    lastMaybe,
    monthKey,
    periodsFromDateSpan,
    periodsFromReport,
    takeLast,
 )
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (Conversion (..), conversionFromMixed, missingCommodities)

data TimelinePoint = TimelinePoint
    { period :: Text
    , startDate :: Day
    , positive :: Decimal
    , negative :: Decimal
    , net :: Decimal
    , missing :: Set Text
    }

data BreakdownItem = BreakdownItem
    { account :: Text
    , quantity :: Decimal
    }

{- | The rows and per-column totals a calculation projects out of a periodic
report. A report built for one set of account types can use hledger's own totals
row; a consolidated report cannot, because hledger derives that row from the
root account's inclusive balance, which spans every side at once.
-}
data ReportSide = ReportSide
    { columns :: [Hledger.MixedAmount]
    , rows :: [HledgerMultiBalance.MultiBalanceReportRow]
    }

wholeReport :: HledgerMultiBalance.MultiBalanceReport -> ReportSide
wholeReport report = ReportSide{columns = report.prTotals.prrAmounts, rows = report.prRows}

{- | One side of a consolidated report, selected by account type.

hledger clips account names on the postings before it aggregates, so a row's
balance already includes everything beneath it and summing the retained rows is
exact at any depth. The consolidated report is nonetheless built unclipped: a
clipped name spanning both sides could not be attributed to either.
-}
reportSide ::
    Hledger.Journal ->
    [Hledger.AccountType] ->
    HledgerMultiBalance.MultiBalanceReport ->
    ReportSide
reportSide journal accountTypes report =
    ReportSide{columns = columnSums (length report.prDates) kept, rows = kept}
  where
    kept = filter matchesType report.prRows
    matchesType row =
        maybe
            False
            (`elem` accountTypes)
            (HledgerJournal.journalAccountType journal row.prrName.displayFull)

columnSums :: Int -> [HledgerMultiBalance.MultiBalanceReportRow] -> [Hledger.MixedAmount]
columnSums columnCount =
    List.foldl' (zipWith HledgerAmount.maPlus) (replicate columnCount HledgerAmount.nullmixedamt)
        . fmap (.prrAmounts)

periodicBalanceReport ::
    Snapshot ->
    [Hledger.Transaction] ->
    Granularity ->
    Text ->
    [Hledger.AccountType] ->
    HledgerReportOptions.BalanceCalculation ->
    HledgerReportOptions.BalanceAccumulation ->
    -- | Display depth to clip rows to; 'Nothing' leaves the report unclipped.
    Maybe Int ->
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
        periodicBalanceReportIn
            snapshot
            transactions
            granularity
            (Just valuation)
            accountTypes
            calculation
            accumulation
            depth
            Nothing
      where
        valuation = case accumulation of
            HledgerReportOptions.PerPeriod -> HledgerValuation.AtThen (Just commodity)
            HledgerReportOptions.Cumulative -> HledgerValuation.AtEnd (Just commodity)
            HledgerReportOptions.Historical -> HledgerValuation.AtEnd (Just commodity)

{- | 'periodicBalanceReport' with the valuation and the column window given
explicitly.

An unvalued report is what a checkpointed calculation needs: with @value_@ unset
hledger reports raw per-account amounts, which are the only append-stable thing a
checkpoint can hold. An explicit window lets it ask for a suffix of the columns
the default window would produce.
-}
periodicBalanceReportIn ::
    Snapshot ->
    [Hledger.Transaction] ->
    Granularity ->
    -- | Valuation to apply; 'Nothing' leaves the amounts unvalued.
    Maybe HledgerValuation.ValuationType ->
    [Hledger.AccountType] ->
    HledgerReportOptions.BalanceCalculation ->
    HledgerReportOptions.BalanceAccumulation ->
    -- | Display depth to clip rows to; 'Nothing' leaves the report unclipped.
    Maybe Int ->
    -- | Inclusive column window; 'Nothing' uses the default window.
    Maybe (Day, Day) ->
    HledgerMultiBalance.MultiBalanceReport
periodicBalanceReportIn
    snapshot
    transactions
    granularity
    valuation
    accountTypes
    calculation
    accumulation
    depth
    window =
        HledgerMultiBalance.multiBalanceReport reportSpec filteredJournal
      where
        reportPeriod = case window of
            Just (from, to) -> Hledger.PeriodBetween from (addDays 1 to)
            Nothing ->
                windowPeriod (selectedWindow granularity (windowColumns snapshot transactions granularity accountTypes))
        baseReportOptions = balanceOptions granularity calculation accumulation depth
        reportOptions =
            baseReportOptions
                { HledgerReportOptions.period_ = reportPeriod
                , HledgerReportOptions.value_ = valuation
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
        filteredJournal = reportJournal snapshot transactions

{- | The columns 'periodicBalanceReport' produces for these inputs: every period
for 'Yearly', the last 24 for 'Monthly'.

A resumed calculation needs these before it builds any report, because they
determine both the day each column is valued at and the day a checkpoint has to
open from.
-}
periodicBalanceColumns ::
    Snapshot ->
    [Hledger.Transaction] ->
    Granularity ->
    [Hledger.AccountType] ->
    [ReportPeriod]
periodicBalanceColumns snapshot transactions granularity accountTypes =
    selectedWindow granularity (windowColumns snapshot transactions granularity accountTypes)

{- | The journal a periodic report is built over: the requested transactions, and
price directives truncated to the latest journal date.

The truncation is load-bearing and exposed for that reason. hledger expands the
final interval to its calendar boundary, which can fall after the latest journal
date, so a directive dated in between would be visible to a resumed calculation
that valued columns from the snapshot's own journal and invisible to the report
that calculation has to agree with.
-}
reportJournal :: Snapshot -> [Hledger.Transaction] -> Hledger.Journal
reportJournal snapshot transactions =
    snapshot.journal
        { Hledger.jtxns = transactions
        , Hledger.jpricedirectives = pricesThroughLatest
        }
  where
    pricesThroughLatest = case snapshot.latestJournalDate of
        Nothing -> snapshot.journal.jpricedirectives
        Just latest -> filter ((<= latest) . Hledger.pddate) snapshot.journal.jpricedirectives

{- | Valuation into a display commodity as at a date, for a report journal.

This is the valuation hledger itself applies to a @Historical@ report's
accumulated columns: @mixedAmountValueAtDate@ over the journal's own price oracle
and display styles. It is spelled out here, rather than reached through
'Hedger.Domain.Valuation.valueMixedAt', for two reasons: that function values
against the snapshot's untruncated journal, and it rebuilds the price oracle per
date, where a resumed report values one column per month.
-}
reportValuation :: Hledger.Journal -> Text -> (Day -> Hledger.MixedAmount -> Hledger.MixedAmount)
reportValuation journal commodity =
    HledgerValuation.mixedAmountValueAtDate
        (HledgerValuation.journalPriceOracle False journal)
        (HledgerJournal.journalCommodityStyles journal)
        (Just commodity)

reportWindow ::
    Granularity ->
    Hledger.Journal ->
    HledgerReportOptions.ReportSpec ->
    (Hledger.Period, Hledger.Interval)
reportWindow granularity journal reportSpec =
    ( windowPeriod (selectedWindow granularity (periodsFromReport granularity journal reportSpec))
    , intervalFor granularity
    )

windowPeriod :: [ReportPeriod] -> Hledger.Period
windowPeriod columns = case (columns, lastMaybe columns) of
    (first : _, Just final) -> Hledger.PeriodBetween first.start (addDays 1 final.end)
    _ -> Hledger.PeriodAll

selectedWindow :: Granularity -> [ReportPeriod] -> [ReportPeriod]
selectedWindow granularity periods = case granularity of
    Monthly -> takeLast 24 periods
    Yearly -> periods

{- | Every period the journal spans, before the window selection narrows it.

The spec this asks hledger for is deliberately the report's own: the window
depends on the requested transactions and account types, because an open-ended
date query is filled in from the span of the journal they leave behind.
-}
windowColumns ::
    Snapshot ->
    [Hledger.Transaction] ->
    Granularity ->
    [Hledger.AccountType] ->
    [ReportPeriod]
windowColumns snapshot transactions granularity accountTypes =
    periodsFromReport granularity (reportJournal snapshot transactions) windowSpec
  where
    windowSpec =
        HledgerReportOptions.defreportspec
            { HledgerReportOptions._rsReportOpts =
                balanceOptions granularity HledgerReportOptions.CalcChange HledgerReportOptions.Historical Nothing
            , HledgerReportOptions._rsDay = fromMaybe epoch snapshot.latestJournalDate
            , HledgerReportOptions._rsQuery = windowQuery
            }
    windowQuery =
        HledgerQuery.And
            ( HledgerQuery.Type accountTypes
                : case snapshot.latestJournalDate of
                    Just latest ->
                        [ HledgerQuery.Date
                            (HledgerPeriod.periodAsDateSpan (Hledger.PeriodTo (addDays 1 latest)))
                        ]
                    Nothing -> []
            )

balanceOptions ::
    Granularity ->
    HledgerReportOptions.BalanceCalculation ->
    HledgerReportOptions.BalanceAccumulation ->
    Maybe Int ->
    HledgerReportOptions.ReportOpts
balanceOptions granularity calculation accumulation depth =
    HledgerReportOptions.defreportopts
        { HledgerReportOptions.interval_ = intervalFor granularity
        , HledgerReportOptions.depth_ = Hledger.DepthSpec (max 1 <$> depth) []
        , HledgerReportOptions.empty_ = True
        , HledgerReportOptions.balancecalc_ = calculation
        , HledgerReportOptions.balanceaccum_ = accumulation
        , HledgerReportOptions.accountlistmode_ = HledgerReportOptions.ALFlat
        }

reportPeriodsFrom :: Granularity -> HledgerMultiBalance.MultiBalanceReport -> [ReportPeriod]
reportPeriodsFrom granularity report = concatMap (periodsFromDateSpan granularity) report.prDates

periodsWithData ::
    Granularity ->
    [Hledger.Transaction] ->
    [ReportPeriod] ->
    [ReportPeriod]
periodsWithData granularity transactions periods =
    case granularity of
        Monthly ->
            let dataMonths = Set.fromList (fmap (monthKey . (.tdate)) transactions)
             in filter ((`Set.member` dataMonths) . (.key)) periods
        Yearly -> periods

reportTotals :: Text -> ReportSide -> [Conversion Decimal]
reportTotals commodity side = fmap (conversionFromMixed commodity) side.columns

reportMissing :: Text -> ReportSide -> Set Text
reportMissing commodity side =
    Set.unions
        [ missingCommodities commodity amount
        | amount <- side.columns <> concatMap (.prrAmounts) side.rows
        ]

selectedPeriod :: Maybe Text -> [ReportPeriod] -> Maybe ReportPeriod
selectedPeriod requested periods =
    case requested >>= (\key -> List.find ((== key) . (.key)) periods) of
        Just period -> Just period
        Nothing -> lastMaybe periods

{- | Ungrouped, depth-collapsed per-account totals for the selected period. The
top-N / \"Other\" grouping is a presentation concern applied downstream.
-}
breakdownItemsFor ::
    Text ->
    Int ->
    Maybe ReportPeriod ->
    [ReportPeriod] ->
    ReportSide ->
    (Text, [BreakdownItem])
breakdownItemsFor commodity depth selected periods side =
    case selected of
        Nothing -> ("", [])
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
                        side.rows
                depthGrouped =
                    [ BreakdownItem{account, quantity}
                    | (account, quantity) <-
                        Map.toList
                            (Map.fromListWith (+) [(atDepth depth item.account, item.quantity) | item <- raw])
                    ]
             in (period.key, depthGrouped)
