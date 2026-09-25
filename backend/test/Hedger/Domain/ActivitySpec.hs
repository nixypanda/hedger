module Hedger.Domain.ActivitySpec (
    activitySpec,
) where

import Data.Decimal (Decimal)
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, addDays, addGregorianMonthsClip, fromGregorian)
import Hedgehog (Gen, Property, assert, failure, footnote, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Test.Tasty.Hedgehog (testProperty)

import Hledger.Data.Amount (mixed, num)
import Hledger.Data.Posting (nullposting)
import Hledger.Data.Transaction (nulltransaction)
import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Activity (
    ActivityBucketFigures (..),
    ActivityDirection (..),
    ActivityFigures (..),
    ActivityKind (..),
    ActivityRange (..),
    BreakdownItem (..),
    computeActivity,
    parseActivityPeriod,
 )
import Hedger.Domain.Period (ReportPeriod (..), monthKey)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Generators (displayCommodity, formatYearMonth, genMonth, genSnapshot, genYear)
import Hedger.SyntheticTransactions (mkSnapshot)
import Hedger.TestHelpers (sumQuantities)

activitySpec :: TestTree
activitySpec =
    testGroup
        "activity"
        [ testGroup
            "parseActivityPeriod MonthActivity"
            [ testProperty
                "a canonical YYYY-MM is the corresponding calendar month"
                monthActivityValid
            , testProperty "a malformed YYYY-MM is rejected" monthActivityMalformed
            ]
        , testGroup
            "parseActivityPeriod FiscalYearActivity"
            [ testProperty "the fiscal-start month yields a 12-month period" fiscalYearValid
            , testProperty "a month other than the fiscal start is rejected" fiscalYearMismatch
            , testProperty "a malformed YYYY-MM is rejected" fiscalYearMalformed
            ]
        , testGroup
            "computeActivity direction breakdown"
            [ testCase "asset activity splits a day into acquire/rebalance/dispose" assetDirections
            , testCase "liability activity splits borrowing from pay-off" liabilityDirections
            ]
        , testGroup
            "computeActivity flow breakdown"
            [ testCase
                "expense activity includes purchases funded by liabilities"
                liabilityFundedExpense
            , testCase "income activity includes gains from asset disposals" disposalGainIncome
            ]
        , testGroup
            "activity conservation"
            [ testProperty
                "category magnitudes are invariant under display depth"
                categoryDepthInvariance
            ]
        ]

assetDirections :: Assertion
assetDirections =
    assertEqual
        "asset directions"
        [ (Increase, [("assets:equity", 100)])
        , (Reallocate, [("assets:debt", 30), ("assets:equity", 30)])
        , (Decrease, [("assets:equity", 50)])
        ]
        (directionBreakdown AssetActivity assetTransactions)

liabilityDirections :: Assertion
liabilityDirections =
    assertEqual
        "liability directions"
        [(Increase, [("liabilities:card", 200)]), (Decrease, [("liabilities:card", 200)])]
        (directionBreakdown LiabilityActivity liabilityTransactions)

liabilityFundedExpense :: Assertion
liabilityFundedExpense =
    assertEqual
        "expense activity"
        [(100, [("expenses:food", 100)])]
        (activityBreakdown ExpenseActivity liabilityFundedExpenseTransactions)

disposalGainIncome :: Assertion
disposalGainIncome =
    assertEqual
        "income activity"
        [(10, [("income:capital:gains", 10)])]
        (activityBreakdown IncomeActivity disposalGainTransactions)

activityDay :: Day
activityDay = fromGregorian 2024 5 24

posting :: Text -> Integer -> Hledger.Posting
posting account amount =
    nullposting
        { Hledger.paccount = account
        , Hledger.pamount = mixed [(num (fromInteger amount)){Hledger.acommodity = "R"}]
        }

transactionOn :: [Hledger.Posting] -> Hledger.Transaction
transactionOn postings = nulltransaction{Hledger.tdate = activityDay, Hledger.tpostings = postings}

assetTransactions :: [Hledger.Transaction]
assetTransactions =
    [ transactionOn [posting "assets:equity" 100, posting "assets:cash" (-100)]
    , transactionOn [posting "assets:equity" (-50), posting "assets:cash" 50]
    , transactionOn [posting "assets:equity" (-30), posting "assets:debt" 30]
    ]

liabilityTransactions :: [Hledger.Transaction]
liabilityTransactions =
    [ transactionOn [posting "liabilities:card" (-200), posting "assets:cash" 200]
    , transactionOn [posting "liabilities:card" 200, posting "assets:cash" (-200)]
    ]

liabilityFundedExpenseTransactions :: [Hledger.Transaction]
liabilityFundedExpenseTransactions =
    [transactionOn [posting "expenses:food" 100, posting "liabilities:card" (-100)]]

disposalGainTransactions :: [Hledger.Transaction]
disposalGainTransactions =
    [ transactionOn
        [ posting "assets:investments:stock" (-100)
        , posting "assets:cash" 110
        , posting "income:capital:gains" (-10)
        ]
    ]

activityBreakdown :: ActivityKind -> [Hledger.Transaction] -> [(Decimal, [(Text, Decimal)])]
activityBreakdown kind transactions =
    case activityFigures kind transactions of
        bucket : _ ->
            [
                ( bucket.total
                , [(item.account, item.quantity) | item <- bucket.categoryItems]
                )
            ]
        [] -> []

directionBreakdown ::
    ActivityKind -> [Hledger.Transaction] -> [(ActivityDirection, [(Text, Decimal)])]
directionBreakdown kind transactions =
    case activityFigures kind transactions of
        bucket : _ -> fmap summariseDirection bucket.directionItems
        [] -> []

activityFigures :: ActivityKind -> [Hledger.Transaction] -> [ActivityBucketFigures]
activityFigures kind transactions =
    case parseActivityPeriod 1 MonthActivity "2024-05" of
        Left _ -> []
        Right activityPeriod ->
            ( computeActivity
                (mkSnapshot transactions)
                transactions
                "R"
                kind
                MonthActivity
                3
                activityPeriod
            ).buckets

summariseDirection :: (ActivityDirection, [BreakdownItem]) -> (ActivityDirection, [(Text, Decimal)])
summariseDirection (direction, items) = (direction, [(item.account, item.quantity) | item <- items])

genFiscalStart :: Gen Int
genFiscalStart = Gen.integral (Range.linear 1 12)

monthActivityValid :: Property
monthActivityValid =
    property $ do
        year <- forAll genYear
        month <- forAll genMonth
        fiscalStart <- forAll genFiscalStart
        let raw = formatYearMonth year month
            expectedStart = fromGregorian year month 1
        case parseActivityPeriod fiscalStart MonthActivity raw of
            Left err -> footnote (show err) >> failure
            Right period -> do
                period.start === expectedStart
                assert (period.start <= period.end)
                period.key === monthKey period.start
                addDays 1 period.end === addGregorianMonthsClip 1 expectedStart

genMalformedYearMonth :: Gen Text
genMalformedYearMonth = Gen.choice [genUnpaddedMonth, genOutOfRangeMonth, genWrongComponentCount]
  where
    genUnpaddedMonth = do
        year <- genYear
        month <- Gen.integral (Range.linear 1 9)
        pure (Text.pack (show year <> "-" <> show (month :: Int)))
    genOutOfRangeMonth = do
        year <- genYear
        month <- Gen.choice [Gen.integral (Range.linear (-50) 0), Gen.integral (Range.linear 13 99)]
        pure (Text.pack (show year <> "-" <> show (month :: Int)))
    genWrongComponentCount = do
        year <- genYear
        month <- genMonth
        let padded = formatYearMonth year month
        Gen.choice
            [ pure (Text.filter (/= '-') padded)
            , pure (padded <> "-01")
            , pure ("-" <> padded)
            , pure ""
            ]

monthActivityMalformed :: Property
monthActivityMalformed =
    property $ do
        fiscalStart <- forAll genFiscalStart
        raw <- forAll genMalformedYearMonth
        assert (isLeft (parseActivityPeriod fiscalStart MonthActivity raw))

fiscalYearValid :: Property
fiscalYearValid =
    property $ do
        fiscalStart <- forAll genFiscalStart
        year <- forAll genYear
        let raw = formatYearMonth year fiscalStart
        case parseActivityPeriod fiscalStart FiscalYearActivity raw of
            Left err -> footnote (show err) >> failure
            Right period -> do
                assert (period.start <= period.end)
                addGregorianMonthsClip 12 period.start === addDays 1 period.end

fiscalYearMismatch :: Property
fiscalYearMismatch =
    property $ do
        fiscalStart <- forAll genFiscalStart
        month <- forAll (Gen.filter (/= fiscalStart) genMonth)
        year <- forAll genYear
        let raw = formatYearMonth year month
        assert (isLeft (parseActivityPeriod fiscalStart FiscalYearActivity raw))

fiscalYearMalformed :: Property
fiscalYearMalformed =
    property $ do
        fiscalStart <- forAll genFiscalStart
        raw <- forAll genMalformedYearMonth
        assert (isLeft (parseActivityPeriod fiscalStart FiscalYearActivity raw))

-- Conservation properties

activityDepths :: [Int]
activityDepths = [1, 2, 3, 4]

activityKinds :: [ActivityKind]
activityKinds = [ExpenseActivity, IncomeActivity, AssetActivity, LiabilityActivity]

categoryDepthInvariance :: Property
categoryDepthInvariance =
    property $ do
        snapshot <- forAll (Gen.filter (isJust . firstActivityWithBuckets) genSnapshot)
        case firstActivityWithBuckets snapshot of
            Nothing -> footnote "generated snapshot has no activity buckets" >> failure
            Just (kind, period) -> do
                let totalsAt depth =
                        Map.fromList
                            [ (bucket.key, sumQuantities bucket.categoryItems)
                            | bucket <- activityFiguresFor snapshot kind period depth
                            ]
                for_ activityDepths $ \depth -> do
                    footnote ("display depth " <> show depth)
                    totalsAt depth === totalsAt 1

-- The month of each transaction, earliest first, as activity periods.
transactionMonths :: Snapshot -> [ReportPeriod]
transactionMonths snapshot =
    mapMaybe
        (either (const Nothing) Just . parseActivityPeriod 1 MonthActivity)
        (Set.toAscList (Set.fromList (fmap (monthKey . (.tdate)) snapshot.transactions)))

{- | The first month, and kind, that produce at least one activity bucket. The
depth-invariance property uses it so it never exercises an empty month.
-}
firstActivityWithBuckets :: Snapshot -> Maybe (ActivityKind, ReportPeriod)
firstActivityWithBuckets = firstActivityWithBucketsOver activityKinds

firstActivityWithBucketsOver :: [ActivityKind] -> Snapshot -> Maybe (ActivityKind, ReportPeriod)
firstActivityWithBucketsOver kinds snapshot =
    case [ (kind, period)
         | period <- transactionMonths snapshot
         , kind <- kinds
         , not (null (activityFiguresFor snapshot kind period 1))
         ] of
        sample : _ -> Just sample
        [] -> Nothing

activityFiguresFor :: Snapshot -> ActivityKind -> ReportPeriod -> Int -> [ActivityBucketFigures]
activityFiguresFor snapshot kind period depth =
    ( computeActivity
        snapshot
        snapshot.transactions
        displayCommodity
        kind
        MonthActivity
        depth
        period
    ).buckets
