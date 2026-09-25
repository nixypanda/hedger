{- | The consolidated-report claim: selecting one side out of a report built over
several account types must reproduce, column for column, exactly what a report
filtered to those types alone produced.
-}
module Hedger.Domain.PeriodReportSpec (
    periodReportSpec,
) where

import Data.Decimal (Decimal)
import Data.Foldable (traverse_)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Internal.Cashflow (
    expensePeriodicReport,
    incomeExpensePeriodicReport,
    incomeGainPeriodicReport,
 )
import Hedger.Domain.Internal.NetWorth (
    assetCashPeriodicReport,
    assetLiabilityPeriodicReport,
    liabilityPeriodicReport,
 )
import Hedger.Domain.Internal.PeriodReport (
    BreakdownItem (..),
    ReportSide,
    breakdownItemsFor,
    reportMissing,
    reportPeriodsFrom,
    reportSide,
    reportTotals,
    wholeReport,
 )
import Hedger.Domain.Period (Granularity (..), ReportPeriod, lastMaybe)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (Conversion (..))
import Hedger.Infrastructure.SnapshotStore (loadSnapshot)

periodReportSpec :: TestTree
periodReportSpec =
    testGroup
        "period report"
        [ testGroup
            "a consolidated side agrees with its separately filtered report"
            [ testCase (label journal granularity commodity depth) $
                sidesAgree journal granularity commodity depth
            | journal <- fixtures
            , granularity <- [Monthly, Yearly]
            , commodity <- ["INR", "USD"]
            , depth <- [2]
            ]
        ]
  where
    label journal granularity commodity depth =
        Text.unpack journal
            <> " "
            <> show granularity
            <> " "
            <> Text.unpack commodity
            <> " depth "
            <> show depth

{- | Journals covering missing prices, foreign cash, dust residuals, nested
mappings and gapped price histories: the cases where a per-side total, its
missing set, or its depth collapse could diverge from the consolidated report.
-}
fixtures :: [Text]
fixtures =
    [ "test/fixtures/net-worth-future-price.journal"
    , "test/fixtures/shared.journal"
    , "test/fixtures/investments.journal"
    , "test/fixtures/investment-never-priced.journal"
    , "test/fixtures/investment-gapped-price.journal"
    , "test/fixtures/investment-foreign-cash.journal"
    , "test/fixtures/investment-dust-commodity.journal"
    , "test/fixtures/investment-nested-mapping.journal"
    , "test/fixtures/investment-gain-fee-roles.journal"
    ]

sidesAgree :: Text -> Granularity -> Text -> Int -> IO ()
sidesAgree path granularity commodity depth = do
    snapshot <- loadSnapshot path >>= either (assertFailure . show) pure
    let transactions = snapshot.transactions
        balances = assetLiabilityPeriodicReport snapshot transactions granularity commodity
        flows = incomeExpensePeriodicReport snapshot transactions granularity commodity
        separate report = wholeReport (report snapshot transactions granularity commodity depth)
        side = reportSide snapshot.journal
    traverse_
        (agrees commodity depth (reportPeriodsFrom granularity balances))
        [ ("assets", side [Hledger.Asset, Hledger.Cash] balances, separate assetCashPeriodicReport)
        , ("liabilities", side [Hledger.Liability] balances, separate liabilityPeriodicReport)
        ]
    traverse_
        (agrees commodity depth (reportPeriodsFrom granularity flows))
        [ ("income", side [Hledger.Revenue, Hledger.Gain] flows, separate incomeGainPeriodicReport)
        , ("expenses", side [Hledger.Expense] flows, separate expensePeriodicReport)
        ]

agrees :: Text -> Int -> [ReportPeriod] -> (String, ReportSide, ReportSide) -> IO ()
agrees commodity depth periods (side, consolidated, separate) = do
    assertEqual
        (side <> " column totals")
        (columnValues (reportTotals commodity separate))
        (columnValues (reportTotals commodity consolidated))
    assertEqual
        (side <> " missing commodities")
        (reportMissing commodity separate)
        (reportMissing commodity consolidated)
    assertEqual
        (side <> " breakdown")
        (breakdownValues (breakdown separate))
        (breakdownValues (breakdown consolidated))
  where
    breakdown = breakdownItemsFor commodity depth (lastMaybe periods) periods

columnValues :: [Conversion Decimal] -> [(Maybe Decimal, Set Text)]
columnValues = fmap (\column -> (column.value, column.missing))

breakdownValues :: (Text, [BreakdownItem]) -> (Text, [(Text, Decimal)])
breakdownValues (period, items) = (period, [(item.account, item.quantity) | item <- items])
