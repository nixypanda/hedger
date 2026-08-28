{- | The consolidated-report claim: selecting one side out of a report built over
several account types must reproduce, column for column, exactly what a report
filtered to those types alone produced.
-}
module Hedger.Domain.PeriodReportSpec (periodReportSpec) where

import Data.Decimal (Decimal)
import Data.Foldable (traverse_)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (fromGregorian)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

import Hledger.Data.Transaction (nulltransaction)
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
    periodsWithData,
    reportMissing,
    reportPeriodsFrom,
    reportSide,
    reportTotals,
    wholeReport,
 )
import Hedger.Domain.Period (Granularity (..), ReportPeriod (key), lastMaybe, monthPeriod)
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
            , depth <- [1, 2, 3]
            ]
        , testGroup
            "periods with data"
            [ testCase "distinguishes December from January" decemberJanuaryPeriods
            , testCase "distinguishes the same month across years" sameMonthDifferentYears
            , testCase "deduplicates repeated months" repeatedMonths
            , testCase "returns no months for empty input" emptyTransactions
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

decemberJanuaryPeriods :: IO ()
decemberJanuaryPeriods =
    assertEqual
        "only December is retained"
        ["2024-12"]
        ( fmap
            (.key)
            ( periodsWithData
                Monthly
                [transactionOn 2024 12 31]
                [monthPeriod (fromGregorian 2024 12 1), monthPeriod (fromGregorian 2025 1 1)]
            )
        )

sameMonthDifferentYears :: IO ()
sameMonthDifferentYears =
    assertEqual
        "only the matching year is retained"
        ["2025-03"]
        ( fmap
            (.key)
            ( periodsWithData
                Monthly
                [transactionOn 2025 3 15]
                [monthPeriod (fromGregorian 2024 3 1), monthPeriod (fromGregorian 2025 3 1)]
            )
        )

repeatedMonths :: IO ()
repeatedMonths =
    assertEqual
        "the matching period appears once"
        ["2024-06"]
        ( fmap
            (.key)
            ( periodsWithData
                Monthly
                [transactionOn 2024 6 1, transactionOn 2024 6 30]
                [monthPeriod (fromGregorian 2024 6 1)]
            )
        )

emptyTransactions :: IO ()
emptyTransactions =
    assertEqual
        "no period has data"
        []
        (fmap (.key) (periodsWithData Monthly [] [monthPeriod (fromGregorian 2024 6 1)]))

transactionOn :: Integer -> Int -> Int -> Hledger.Transaction
transactionOn year month day =
    nulltransaction{Hledger.tdate = fromGregorian year month day}

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
