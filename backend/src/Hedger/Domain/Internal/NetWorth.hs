{- | Net-worth-specific report construction. Keeping these report definitions
here lets the calculation and its component benchmarks exercise the same
hledger options.
-}
module Hedger.Domain.Internal.NetWorth (
    assetCashPeriodicReport,
    liabilityPeriodicReport,
    gainPeriodicReport,
    currentAssetCashBalance,
    currentLiabilityBalance,
)
where

import Data.Decimal (Decimal)
import Data.Text (Text)
import Data.Time (addDays)

import Hledger.Data.Period qualified as HledgerPeriod
import Hledger.Data.Types qualified as Hledger
import Hledger.Data.Valuation qualified as HledgerValuation
import Hledger.Query qualified as HledgerQuery
import Hledger.Reports.BalanceReport qualified as HledgerBalance
import Hledger.Reports.MultiBalanceReport qualified as HledgerMultiBalance
import Hledger.Reports.ReportOptions qualified as HledgerReportOptions

import Hedger.Domain.Internal.PeriodReport (periodicBalanceReport)
import Hedger.Domain.Period (Granularity)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (Conversion, complete, conversionFromMixed, requireComplete)

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
        depth

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
        depth

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
        1

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
