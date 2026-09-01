{- | Net-worth-specific report construction. Keeping these report definitions
here lets the calculation and its component benchmarks exercise the same
hledger options.
-}
module Hedger.Domain.Internal.NetWorth (
    assetLiabilityAccountTypes,
    assetLiabilityPeriodicReport,
    assetLiabilityDeltaReport,
    assetCashPeriodicReport,
    liabilityPeriodicReport,
    gainPeriodicReport,
    currentAssetCashBalance,
    currentLiabilityBalance,
)
where

import Data.Decimal (Decimal)
import Data.Text (Text)
import Data.Time (Day, addDays)

import Hledger.Data.Period qualified as HledgerPeriod
import Hledger.Data.Types qualified as Hledger
import Hledger.Data.Valuation qualified as HledgerValuation
import Hledger.Query qualified as HledgerQuery
import Hledger.Reports.BalanceReport qualified as HledgerBalance
import Hledger.Reports.MultiBalanceReport qualified as HledgerMultiBalance
import Hledger.Reports.ReportOptions qualified as HledgerReportOptions

import Hedger.Domain.Internal.PeriodReport (periodicBalanceReport, periodicBalanceReportIn)
import Hedger.Domain.Period (Granularity (Monthly))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (Conversion, complete, conversionFromMixed, requireComplete)

-- | The account types the consolidated balance-sheet report spans.
assetLiabilityAccountTypes :: [Hledger.AccountType]
assetLiabilityAccountTypes = [Hledger.Asset, Hledger.Cash, Hledger.Liability]

{- | Assets, cash and liabilities in a single report; each side is selected out
of it afterwards by account type. Built unclipped so that summing a side's rows
still reproduces that side's totals.
-}
assetLiabilityPeriodicReport ::
    Snapshot ->
    [Hledger.Transaction] ->
    Granularity ->
    Text ->
    HledgerMultiBalance.MultiBalanceReport
assetLiabilityPeriodicReport snapshot transactions granularity commodity =
    periodicBalanceReport
        snapshot
        transactions
        granularity
        commodity
        assetLiabilityAccountTypes
        HledgerReportOptions.CalcChange
        HledgerReportOptions.Historical
        Nothing

{- | Per-account, per-month, unvalued balance /changes/ over an inclusive window.

@PerPeriod@ rather than @Historical@ so that no starting balance is folded in:
each column is the change within that month alone, which is what a checkpointed
cumulative balance can be extended by. Unvalued, because only a raw cumulative
balance is stable under a later append — a valued one is not, since the price
that values it may not have been written yet.

Monthly whatever granularity the request asked for. A year end is also a month
end, so monthly changes can be accumulated up to either kind of column end, and
one shape of delta report serves both.
-}
assetLiabilityDeltaReport ::
    Snapshot ->
    [Hledger.Transaction] ->
    -- | Inclusive window to report changes over.
    (Day, Day) ->
    HledgerMultiBalance.MultiBalanceReport
assetLiabilityDeltaReport snapshot transactions window =
    periodicBalanceReportIn
        snapshot
        transactions
        Monthly
        Nothing
        assetLiabilityAccountTypes
        HledgerReportOptions.CalcChange
        HledgerReportOptions.PerPeriod
        Nothing
        (Just window)

assetCashPeriodicReport ::
    Snapshot ->
    [Hledger.Transaction] ->
    Granularity ->
    Text ->
    Int ->
    HledgerMultiBalance.MultiBalanceReport
assetCashPeriodicReport snapshot transactions granularity commodity depth =
    periodicBalanceReport
        snapshot
        transactions
        granularity
        commodity
        [Hledger.Asset, Hledger.Cash]
        HledgerReportOptions.CalcChange
        HledgerReportOptions.Historical
        (Just depth)

liabilityPeriodicReport ::
    Snapshot ->
    [Hledger.Transaction] ->
    Granularity ->
    Text ->
    Int ->
    HledgerMultiBalance.MultiBalanceReport
liabilityPeriodicReport snapshot transactions granularity commodity depth =
    periodicBalanceReport
        snapshot
        transactions
        granularity
        commodity
        [Hledger.Liability]
        HledgerReportOptions.CalcChange
        HledgerReportOptions.Historical
        (Just depth)

gainPeriodicReport ::
    Snapshot ->
    [Hledger.Transaction] ->
    Granularity ->
    Text ->
    HledgerMultiBalance.MultiBalanceReport
gainPeriodicReport snapshot transactions granularity commodity =
    periodicBalanceReport
        snapshot
        transactions
        granularity
        commodity
        [Hledger.Asset, Hledger.Cash, Hledger.Liability]
        HledgerReportOptions.CalcGain
        HledgerReportOptions.Historical
        (Just 1)

currentAssetCashBalance :: Snapshot -> [Hledger.Transaction] -> Text -> Conversion Decimal
currentAssetCashBalance snapshot transactions commodity =
    currentBalance snapshot transactions commodity [Hledger.Asset, Hledger.Cash]

currentLiabilityBalance :: Snapshot -> [Hledger.Transaction] -> Text -> Conversion Decimal
currentLiabilityBalance snapshot transactions commodity =
    fmap abs (currentBalance snapshot transactions commodity [Hledger.Liability])

currentBalance :: Snapshot -> [Hledger.Transaction] -> Text -> [Hledger.AccountType] -> Conversion Decimal
currentBalance snapshot transactions commodity accountTypes =
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
