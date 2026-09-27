module Hedger.Domain.ActivitySpec (
    activitySpec,
) where

import Data.Decimal (Decimal)
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.List qualified as List
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
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

import Hledger.Data.Amount (mixed, num)
import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Posting (nullposting)
import Hledger.Data.Transaction (nulltransaction)
import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Account (DisplayDepth (..))
import Hedger.Domain.Account qualified as Account
import Hedger.Domain.Activity (
    ActivityBuckets (..),
    ActivityDirection (..),
    ActivityFigures (..),
    ActivityIssue (..),
    ActivityKind (..),
    ActivityRange (..),
    AssetBucketFigures (..),
    BreakdownItem (..),
    ExpenseBucketFigures (..),
    IncomeBucketFigures (..),
    LiabilityBucketFigures (..),
    computeActivity,
    parseActivityPeriod,
 )
import Hedger.Domain.ActivityEvent (
    ActivityEvent (..),
    AssetMovement (..),
    EventGroup (..),
    activityLabelsFor,
    assetAccountName,
    classifyTransactionFor,
 )
import Hedger.Domain.ActivityEvent qualified as ActivityEvent
import Hedger.Domain.Period (
    ReportPeriod,
    monthKey,
    reportPeriodEnd,
    reportPeriodKey,
    reportPeriodStart,
 )
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Transaction qualified as Transaction
import Hedger.Generators (displayCommodity, formatYearMonth, genMonth, genSnapshot, genYear)
import Hedger.Infrastructure.SnapshotStore (loadSnapshot)
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
            , testCase "reversals remain visible at zero net activity" zeroNetReversal
            ]
        , testGroup
            "posting-level activity"
            [ testCase "a cash transfer is one asset transfer" cashTransfer
            , testCase "a financed asset appears in both relevant views" financedAcquisition
            , testCase "multiple financed assets are separate acquisitions" multiFinancedAcquisition
            , testCase
                "the supplied multi-fund journal produces three acquisitions"
                multiFundJournal
            , testCase "priced transfer and single purchase still classify" pricedExistingPatterns
            , testCase "refinancing links repayment and borrowing" refinancing
            , testCase "forgiveness is distinct from repayment" debtForgiveness
            , testCase "a credit-card refund reverses debt and expense" cardRefund
            , testCase "opening balances are excluded" openingExcluded
            , testCase "unpriced transfers do not emit a partial event" unpricedTransfer
            , testCase "ambiguous transfers are reported without guessing" ambiguousTransfer
            ]
        , testGroup
            "activity conservation"
            [ testProperty "emitted amounts and account groups are valid" validEvents
            , testProperty "flow and debt movements conserve posting balances" postingConservation
            , testProperty
                "a transaction occurs once per activity bucket"
                distinctBucketTransactions
            , testProperty
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

cashTransfer :: Assertion
cashTransfer = do
    let transfer = transactionOn [posting "assets:bank:a" (-70), posting "assets:bank:b" 70]
    assertEqual "transfer label" ["asset_transfer"] (activityLabelsFor transfer)
    case activityFigures AssetActivity [transfer] of
        [bucket] -> do
            assertEqual "transfer counted once" 70 bucket.testTotal
            assertEqual "transfer amount" 70 bucket.testReallocated
            assertEqual "one event" 1 bucket.testEventCount
        _ -> assertEqual "one asset bucket" 1 (length (activityFigures AssetActivity [transfer]))

financedAcquisition :: Assertion
financedAcquisition = do
    let purchase = transactionOn [posting "assets:equipment" 100, posting "liabilities:loan" (-100)]
    assertEqual "both labels" ["asset_acquisition", "borrowing"] (activityLabelsFor purchase)
    assertEqual
        "acquired"
        [100]
        [bucket.testIncrease | bucket <- activityFigures AssetActivity [purchase]]
    assertEqual
        "borrowed"
        [100]
        [bucket.testIncrease | bucket <- activityFigures LiabilityActivity [purchase]]

multiFinancedAcquisition :: Assertion
multiFinancedAcquisition = do
    let purchase =
            transactionOn
                [ posting "assets:equity:one" 60
                , posting "assets:equity:two" 40
                , posting "liabilities:loan" (-100)
                ]
    assertEqual "one label per kind" ["asset_acquisition", "borrowing"] (activityLabelsFor purchase)
    case activityFigures AssetActivity [purchase] of
        [bucket] -> do
            assertEqual "both acquisitions contribute" 100 bucket.testIncrease
            assertEqual "two events" 2 bucket.testEventCount
            assertEqual "one drill-down transaction" 1 bucket.testTransactionCount
        _ -> assertFailure "expected one asset bucket"

multiFundJournal :: Assertion
multiFundJournal = do
    result <- loadSnapshot "test/fixtures/activity-multi-asset.journal"
    case result of
        Left loadError -> assertFailure (show loadError)
        Right snapshot ->
            case List.find ((== "Investments India") . (.tdescription)) snapshot.transactions of
                Nothing -> assertFailure "India investment transaction missing"
                Just purchase -> checkMultiFundPurchase snapshot purchase

pricedExistingPatterns :: Assertion
pricedExistingPatterns = do
    result <- loadSnapshot "test/fixtures/activity-multi-asset.journal"
    case result of
        Left loadError -> assertFailure (show loadError)
        Right snapshot -> do
            checkPricedPattern snapshot "Exchange INR to USD" "INR" isTransfer
            checkPricedPattern snapshot "Investments US" "USD" isAcquisition

checkPricedPattern :: Snapshot -> Text -> Text -> (ActivityEvent -> Bool) -> Assertion
checkPricedPattern snapshot description commodity matches =
    case List.find ((== description) . (.tdescription)) snapshot.transactions of
        Nothing -> assertFailure "priced transaction missing"
        Just transaction -> do
            let (events, missing, issues) =
                    classifyTransactionFor AssetGroup snapshot commodity transaction
            assertEqual "one expected event" 1 (length (filter matches events))
            assertEqual "no missing conversion" Set.empty missing
            assertEqual "no classification issue" Set.empty issues

isTransfer :: ActivityEvent -> Bool
isTransfer (AssetEvent (Transferred _ _ _ _)) = True
isTransfer _ = False

isAcquisition :: ActivityEvent -> Bool
isAcquisition (AssetEvent (Acquired _ _)) = True
isAcquisition _ = False

checkMultiFundPurchase :: Snapshot -> Hledger.Transaction -> Assertion
checkMultiFundPurchase snapshot purchase = do
    assertEqual
        "one acquisition label"
        ["expense", "asset_acquisition"]
        (activityLabelsFor purchase)
    let (events, missing, issues) = classifyTransactionFor AssetGroup snapshot "INR" purchase
        accounts =
            [ assetAccountName account
            | AssetEvent (Acquired account _) <- events
            ]
    assertEqual "no conversion missing" Set.empty missing
    assertEqual "no classification issue" Set.empty issues
    assertEqual "three distinct acquisitions" 3 (length accounts)
    case parseActivityPeriod 1 MonthActivity "2026-09" of
        Left err -> assertFailure (show err)
        Right period ->
            case ( computeActivity
                    snapshot
                    [purchase]
                    "INR"
                    AssetActivity
                    MonthActivity
                    Depth4
                    period
                 ).buckets of
                AssetBuckets [bucket] -> do
                    assertEqual "three valued events" 3 (length bucket.movements)
                    assertEqual "one drill-down transaction" 1 (length bucket.transactions)
                    assertBool
                        "acquisition value is near the 400k outflow"
                        (bucket.acquired > 399900)
                    assertBool "brokerage remains outside acquisitions" (bucket.acquired < 400000)
                _ -> assertFailure "expected one asset day bucket"

refinancing :: Assertion
refinancing = do
    let refinance =
            transactionOn
                [ posting "liabilities:old" 80
                , posting "liabilities:new" (-100)
                , posting "assets:cash" 20
                ]
    assertEqual "refinance label" ["refinancing"] (activityLabelsFor refinance)
    case activityFigures LiabilityActivity [refinance] of
        [bucket] -> do
            assertEqual "borrowed leg" 100 bucket.testIncrease
            assertEqual "repaid leg" 80 bucket.testDecrease
            assertEqual "one linked event" 1 bucket.testEventCount
        _ ->
            assertEqual
                "one liability bucket"
                1
                (length (activityFigures LiabilityActivity [refinance]))

debtForgiveness :: Assertion
debtForgiveness = do
    let forgiven = transactionOn [posting "liabilities:loan" 20, posting "income:forgiven" (-20)]
    assertEqual "both labels" ["income", "debt_forgiveness"] (activityLabelsFor forgiven)
    assertEqual
        "forgiven principal"
        [20]
        [bucket.testForgiven | bucket <- activityFigures LiabilityActivity [forgiven]]
    assertEqual
        "income"
        [20]
        [bucket.testPosted | bucket <- activityFigures IncomeActivity [forgiven]]

cardRefund :: Assertion
cardRefund = do
    let refund = transactionOn [posting "liabilities:card" 25, posting "expenses:food" (-25)]
    assertEqual "both labels" ["expense_reversal", "borrowing_reversal"] (activityLabelsFor refund)
    assertEqual
        "debt reversal"
        [25]
        [bucket.testBorrowingReversed | bucket <- activityFigures LiabilityActivity [refund]]
    assertEqual
        "expense reversal"
        [25]
        [bucket.testReversed | bucket <- activityFigures ExpenseActivity [refund]]

zeroNetReversal :: Assertion
zeroNetReversal = do
    let posted = transactionOn [posting "expenses:food" 30, posting "assets:cash" (-30)]
        reversed = transactionOn [posting "expenses:food" (-30), posting "assets:cash" 30]
    case activityFigures ExpenseActivity [posted, reversed] of
        [bucket] -> do
            assertEqual "zero net" 0 bucket.testTotal
            assertEqual "posted" 30 bucket.testPosted
            assertEqual "reversed" 30 bucket.testReversed
            assertEqual "transactions remain visible" 2 bucket.testTransactionCount
        _ ->
            assertEqual
                "one expense bucket"
                1
                (length (activityFigures ExpenseActivity [posted, reversed]))

openingExcluded :: Assertion
openingExcluded = do
    let opening = transactionOn [posting "assets:cash" 100, posting "equity:opening" (-100)]
    assertEqual "no activity labels" [] (activityLabelsFor opening)
    assertEqual "no asset buckets" 0 (length (activityFigures AssetActivity [opening]))

unpricedTransfer :: Assertion
unpricedTransfer = do
    let source = posting "assets:bank:a" (-70)
        destination =
            (posting "assets:bank:b" 70)
                { Hledger.pamount = mixed [(num 70){Hledger.acommodity = "Q"}]
                }
        transfer = transactionOn [source, destination]
        snapshot = mkSnapshot [transfer]
    case parseActivityPeriod 1 MonthActivity "2024-05" of
        Left err -> assertEqual "valid period" "" (show err)
        Right period -> do
            let figures =
                    computeActivity
                        snapshot
                        [transfer]
                        "R"
                        AssetActivity
                        MonthActivity
                        Depth3
                        period
            case figures.buckets of
                AssetBuckets buckets -> assertEqual "no partial transfer" 0 (length buckets)
                _ -> assertEqual "asset result" True False
            assertEqual "missing commodity" (Set.singleton "Q") figures.missing

ambiguousTransfer :: Assertion
ambiguousTransfer = do
    let transfer =
            transactionOn
                [ posting "assets:bank:a" (-100)
                , posting "assets:bank:b" 60
                , posting "assets:bank:c" 40
                ]
        snapshot = mkSnapshot [transfer]
    case parseActivityPeriod 1 MonthActivity "2024-05" of
        Left err -> assertEqual "valid period" "" (show err)
        Right period -> do
            let figures =
                    computeActivity
                        snapshot
                        [transfer]
                        "R"
                        AssetActivity
                        MonthActivity
                        Depth3
                        period
            case figures.buckets of
                AssetBuckets buckets -> assertEqual "no guessed transfer" 0 (length buckets)
                _ -> assertEqual "asset result" True False
            assertEqual "structured issue" (Set.singleton AmbiguousAssetMovement) figures.issues

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
                ( bucket.testTotal
                , [(item.account, item.quantity) | item <- bucket.testCategoryItems]
                )
            ]
        [] -> []

directionBreakdown ::
    ActivityKind -> [Hledger.Transaction] -> [(ActivityDirection, [(Text, Decimal)])]
directionBreakdown kind transactions =
    case activityFigures kind transactions of
        bucket : _ -> fmap summariseDirection bucket.testDirectionItems
        [] -> []

data TestBucket = TestBucket
    { testKey :: Text
    , testTotal :: Decimal
    , testIncrease :: Decimal
    , testDecrease :: Decimal
    , testReallocated :: Decimal
    , testPosted :: Decimal
    , testReversed :: Decimal
    , testForgiven :: Decimal
    , testBorrowingReversed :: Decimal
    , testCategoryItems :: [BreakdownItem]
    , testDirectionItems :: [(ActivityDirection, [BreakdownItem])]
    , testEventCount :: Int
    , testTransactionCount :: Int
    }

toTestBuckets :: ActivityBuckets -> [TestBucket]
toTestBuckets typed =
    case typed of
        AssetBuckets buckets -> fmap assetTestBucket buckets
        LiabilityBuckets buckets -> fmap liabilityTestBucket buckets
        IncomeBuckets buckets -> fmap incomeTestBucket buckets
        ExpenseBuckets buckets -> fmap expenseTestBucket buckets

assetTestBucket :: AssetBucketFigures -> TestBucket
assetTestBucket bucket =
    TestBucket
        { testKey = bucket.key
        , testTotal = bucket.acquired + bucket.disposed + bucket.transferred
        , testIncrease = bucket.acquired
        , testDecrease = bucket.disposed
        , testReallocated = bucket.transferred
        , testPosted = 0
        , testReversed = 0
        , testForgiven = 0
        , testBorrowingReversed = 0
        , testCategoryItems = bucket.categoryItems
        , testDirectionItems = bucket.directionItems
        , testEventCount = length bucket.movements
        , testTransactionCount = length bucket.transactions
        }

liabilityTestBucket :: LiabilityBucketFigures -> TestBucket
liabilityTestBucket bucket =
    TestBucket
        { testKey = bucket.key
        , testTotal = bucket.borrowed + bucket.repaid + bucket.forgiven + bucket.borrowingReversed
        , testIncrease = bucket.borrowed
        , testDecrease = bucket.repaid + bucket.forgiven + bucket.borrowingReversed
        , testReallocated = 0
        , testPosted = 0
        , testReversed = 0
        , testForgiven = bucket.forgiven
        , testBorrowingReversed = bucket.borrowingReversed
        , testCategoryItems = bucket.categoryItems
        , testDirectionItems = bucket.directionItems
        , testEventCount = length bucket.movements
        , testTransactionCount = length bucket.transactions
        }

incomeTestBucket :: IncomeBucketFigures -> TestBucket
incomeTestBucket bucket =
    TestBucket
        { testKey = bucket.key
        , testTotal = bucket.posted - bucket.reversed
        , testIncrease = 0
        , testDecrease = 0
        , testReallocated = 0
        , testPosted = bucket.posted
        , testReversed = bucket.reversed
        , testForgiven = 0
        , testBorrowingReversed = 0
        , testCategoryItems = bucket.categoryItems
        , testDirectionItems = []
        , testEventCount = length bucket.changes
        , testTransactionCount = length bucket.transactions
        }

expenseTestBucket :: ExpenseBucketFigures -> TestBucket
expenseTestBucket bucket =
    TestBucket
        { testKey = bucket.key
        , testTotal = bucket.posted - bucket.reversed
        , testIncrease = 0
        , testDecrease = 0
        , testReallocated = 0
        , testPosted = bucket.posted
        , testReversed = bucket.reversed
        , testForgiven = 0
        , testBorrowingReversed = 0
        , testCategoryItems = bucket.categoryItems
        , testDirectionItems = []
        , testEventCount = length bucket.changes
        , testTransactionCount = length bucket.transactions
        }

activityFigures :: ActivityKind -> [Hledger.Transaction] -> [TestBucket]
activityFigures kind transactions =
    case parseActivityPeriod 1 MonthActivity "2024-05" of
        Left _ -> []
        Right activityPeriod ->
            toTestBuckets
                ( computeActivity
                    (mkSnapshot transactions)
                    transactions
                    "R"
                    kind
                    MonthActivity
                    Depth3
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
                reportPeriodStart period === expectedStart
                reportPeriodKey period === monthKey (reportPeriodStart period)
                addDays 1 (reportPeriodEnd period) === addGregorianMonthsClip 1 expectedStart

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
                addGregorianMonthsClip 12 (reportPeriodStart period)
                    === addDays 1 (reportPeriodEnd period)

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

genActivitySnapshot :: Gen Snapshot
genActivitySnapshot = do
    base <- genSnapshot
    extra <- Gen.list (Range.linear 1 8) genActivityTransaction
    let indexed =
            zipWith
                (\index transaction -> transaction{Hledger.tindex = index})
                [1 ..]
                (base.transactions <> extra)
    pure (mkSnapshot indexed)

genActivityTransaction :: Gen Hledger.Transaction
genActivityTransaction = do
    dayOffset <- Gen.integral (Range.linear 0 397)
    magnitude <- Gen.integral (Range.linear 1 10000)
    variant <- Gen.int (Range.linear 0 5)
    let day = addDays dayOffset (fromGregorian 2024 1 1)
        activityPost account quantity =
            nullposting
                { Hledger.paccount = account
                , Hledger.pamount =
                    mixed [(num (fromInteger quantity)){Hledger.acommodity = displayCommodity}]
                }
        tx postings =
            nulltransaction
                { Hledger.tdate = day
                , Hledger.tpostings =
                    [activityPost account quantity | (account, quantity) <- postings]
                }
    pure $ case variant of
        0 ->
            tx
                [ ("liabilities:old", magnitude)
                , ("liabilities:new", negate (magnitude + 25))
                , ("assets:cash", 25)
                ]
        1 -> tx [("liabilities:loan", magnitude), ("income:forgiven", negate magnitude)]
        2 -> tx [("liabilities:card", magnitude), ("expenses:food", negate magnitude)]
        3 -> tx [("assets:equipment", magnitude), ("liabilities:loan", negate magnitude)]
        4 -> tx [("assets:bank:a", negate magnitude), ("assets:bank:b", magnitude)]
        _ -> tx [("income:salary", magnitude), ("assets:cash", negate magnitude)]

validEvents :: Property
validEvents =
    property $ do
        snapshot <- forAll genActivitySnapshot
        let events = concatMap (valuedEventsFor snapshot) snapshot.transactions
        for_ events (assert . validEvent)

valuedEventsFor :: Snapshot -> Hledger.Transaction -> [ActivityEvent.ActivityEvent]
valuedEventsFor snapshot transaction =
    let (events, _, _) = ActivityEvent.classifyTransaction snapshot displayCommodity transaction
     in events

validEvent :: ActivityEvent.ActivityEvent -> Bool
validEvent event =
    case event of
        ActivityEvent.AssetEvent movement ->
            case movement of
                ActivityEvent.Acquired account amount ->
                    assetAccount account && positive amount
                ActivityEvent.Disposed account amount ->
                    assetAccount account && positive amount
                ActivityEvent.Transferred source outgoing destination incoming ->
                    assetAccount source
                        && assetAccount destination
                        && source /= destination
                        && positive outgoing
                        && positive incoming
        ActivityEvent.LiabilityEvent movement ->
            case movement of
                ActivityEvent.Borrowed account amount -> liabilityAccount account && positive amount
                ActivityEvent.Repaid account amount -> liabilityAccount account && positive amount
                ActivityEvent.Refinanced old repaid new borrowed ->
                    liabilityAccount old
                        && liabilityAccount new
                        && old /= new
                        && positive repaid
                        && positive borrowed
                ActivityEvent.Forgiven account amount -> liabilityAccount account && positive amount
                ActivityEvent.BorrowingReversed account amount ->
                    liabilityAccount account && positive amount
        ActivityEvent.IncomeEvent account change ->
            group "income" (ActivityEvent.incomeAccountName account) && positiveFlow change
        ActivityEvent.ExpenseEvent account change ->
            group "expenses" (ActivityEvent.expenseAccountName account) && positiveFlow change
  where
    positive = (> 0) . ActivityEvent.positiveAmountValue
    positiveFlow = \case
        ActivityEvent.Posted amount -> positive amount
        ActivityEvent.Reversed amount -> positive amount
    group label = (== label) . Account.accountLabel . Account.accountInfo
    assetAccount = group "assets" . ActivityEvent.assetAccountName
    liabilityAccount = group "liabilities" . ActivityEvent.liabilityAccountName

postingConservation :: Property
postingConservation =
    property $ do
        snapshot <- forAll genActivitySnapshot
        let transactions = filter (not . Transaction.isOpening) snapshot.transactions
            events = concatMap (valuedEventsFor snapshot) transactions
            postingsFor label =
                [ amount.aquantity
                | tx <- transactions
                , entry <- tx.tpostings
                , Account.accountLabel (Account.accountInfo entry.paccount) == label
                , amount <- HledgerAmount.amountsRaw entry.pamount
                ]
        sum (fmap incomeChange events) === negate (sum (postingsFor "income"))
        sum (fmap expenseChange events) === sum (postingsFor "expenses")
        sum (fmap debtChange events) === negate (sum (postingsFor "liabilities"))

incomeChange :: ActivityEvent.ActivityEvent -> Decimal
incomeChange event =
    case event of
        ActivityEvent.IncomeEvent _ change -> flowChange change
        _ -> 0

expenseChange :: ActivityEvent.ActivityEvent -> Decimal
expenseChange event =
    case event of
        ActivityEvent.ExpenseEvent _ change -> flowChange change
        _ -> 0

flowChange :: ActivityEvent.FlowChange -> Decimal
flowChange change =
    case change of
        ActivityEvent.Posted amount -> ActivityEvent.positiveAmountValue amount
        ActivityEvent.Reversed amount -> negate (ActivityEvent.positiveAmountValue amount)

debtChange :: ActivityEvent.ActivityEvent -> Decimal
debtChange event =
    case event of
        ActivityEvent.LiabilityEvent movement ->
            case movement of
                ActivityEvent.Borrowed _ amount -> ActivityEvent.positiveAmountValue amount
                ActivityEvent.Repaid _ amount -> negate (ActivityEvent.positiveAmountValue amount)
                ActivityEvent.Refinanced _ repaid _ borrowed ->
                    ActivityEvent.positiveAmountValue borrowed
                        - ActivityEvent.positiveAmountValue repaid
                ActivityEvent.Forgiven _ amount -> negate (ActivityEvent.positiveAmountValue amount)
                ActivityEvent.BorrowingReversed _ amount ->
                    negate (ActivityEvent.positiveAmountValue amount)
        _ -> 0

distinctBucketTransactions :: Property
distinctBucketTransactions =
    property $ do
        snapshot <- forAll genActivitySnapshot
        for_ (transactionMonths snapshot) $ \period ->
            for_ activityKinds $ \kind ->
                for_
                    ( bucketTransactions
                        ( computeActivity
                            snapshot
                            snapshot.transactions
                            displayCommodity
                            kind
                            MonthActivity
                            Depth2
                            period
                        ).buckets
                    )
                    $ \transactions -> do
                        let indices = fmap (.tindex) transactions
                        length indices === length (List.nub indices)

bucketTransactions :: ActivityBuckets -> [[Hledger.Transaction]]
bucketTransactions typed =
    case typed of
        AssetBuckets buckets -> fmap (.transactions) buckets
        LiabilityBuckets buckets -> fmap (.transactions) buckets
        IncomeBuckets buckets -> fmap (.transactions) buckets
        ExpenseBuckets buckets -> fmap (.transactions) buckets

activityDepths :: [DisplayDepth]
activityDepths = [Depth1, Depth2, Depth3, Depth4]

activityKinds :: [ActivityKind]
activityKinds = [ExpenseActivity, IncomeActivity, AssetActivity, LiabilityActivity]

categoryDepthInvariance :: Property
categoryDepthInvariance =
    property $ do
        snapshot <- forAll (Gen.filter (isJust . firstActivityWithBuckets) genActivitySnapshot)
        case firstActivityWithBuckets snapshot of
            Nothing -> footnote "generated snapshot has no activity buckets" >> failure
            Just (kind, period) -> do
                let totalsAt depth =
                        Map.fromList
                            [ ( bucket.testKey
                              ,
                                  ( sumQuantities bucket.testCategoryItems
                                  , bucket.testTotal
                                  , bucket.testIncrease
                                  , bucket.testDecrease
                                  , bucket.testReallocated
                                  , bucket.testPosted
                                  , bucket.testReversed
                                  , bucket.testForgiven
                                  , bucket.testBorrowingReversed
                                  , bucket.testEventCount
                                  )
                              )
                            | bucket <- activityFiguresFor snapshot kind period depth
                            ]
                for_ activityDepths $ \depth -> do
                    footnote ("display depth " <> show depth)
                    totalsAt depth === totalsAt Depth1

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
         , not (null (activityFiguresFor snapshot kind period Depth1))
         ] of
        sample : _ -> Just sample
        [] -> Nothing

activityFiguresFor ::
    Snapshot ->
    ActivityKind ->
    ReportPeriod ->
    DisplayDepth ->
    [TestBucket]
activityFiguresFor snapshot kind period depth =
    toTestBuckets
        ( computeActivity
            snapshot
            snapshot.transactions
            displayCommodity
            kind
            MonthActivity
            depth
            period
        ).buckets
