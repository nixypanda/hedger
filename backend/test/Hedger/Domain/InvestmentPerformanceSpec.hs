module Hedger.Domain.InvestmentPerformanceSpec (
    investmentsSpec,
) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Decimal (Decimal)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Time (Day, fromGregorian)
import Hedgehog (Gen, Property, PropertyT, assert, failure, footnote, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

import Hledger.Data.Transaction (nulltransaction)
import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Internal.InvestmentPerformance (
    InvestmentPerformanceContext (..),
    ScopeScan (..),
    attributedAtWith,
    externalCashFlowsAtWith,
    hasAttributablePerformance,
    holdingsAtWithJournal,
    holdingsSeriesAtWithJournal,
    prepareInvestmentPerformance,
    retainedPerformanceAtWith,
    scopeScansAt,
 )
import Hedger.Domain.Investment (
    AssetAccount,
    AssetClass (Cash, Debt, Equity, Hybrid, Other, Property),
    AssetClassMapping (..),
    AssetClassMappings,
    InvestmentMapping (..),
    InvestmentMappings,
    PerformanceAccount (..),
    PerformanceRole (Fee, Interest, RealisedGain),
    emptyAssetClassMappings,
    emptyInvestmentMappings,
    mkAccountName,
    mkAssetAccount,
    mkAssetClassMappings,
    mkInvestmentMappings,
 )
import Hedger.Domain.InvestmentPerformance (
    InvestmentCompleteness (Complete, Partial, Unavailable),
    InvestmentSummary (..),
    SummaryResult (..),
    externalCashFlowsAt,
    investmentAccountsAt,
    investmentDataTransactions,
    investmentSummaryAt,
    reportPeriods,
    summariesAtPrepared,
    summariesAtScopesPrepared,
    summaryAt,
    summaryAtPrepared,
 )
import Hedger.Domain.Period (Granularity (Monthly, Yearly))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Xirr (CashFlow, cashFlowAmount, cashFlowDate)
import Hedger.Generators (displayCommodity, genSnapshot)
import Hedger.Infrastructure.SnapshotStore (loadSnapshot)

investmentsSpec :: TestTree
investmentsSpec =
    testGroup
        "investment performance"
        [ testCase
            "retained FD interest is income rather than acquisition cost or unrealised gain"
            retainedFdInterest
        , testCase
            "parent gain mapping attributes only the selected FD descendant's interest"
            descendantFdInterest
        , testCase "explicit partial disposal reduces remaining basis" partialDisposal
        , testCase
            "foreign acquisition cost uses acquisition-date FX and inherits its mapping"
            foreignAcquisitionCost
        , testCase
            "monthly FX prices provide historical cost for an uncosted foreign cash balance"
            foreignCashHistoricalCost
        , testCase
            "XIRR cash flows cancel reinvestment and transfers while retaining \
            \external income and fees"
            externalCashFlows
        , testCase
            "investment summary reports annualised XIRR instead of absolute return"
            summaryXirr
        , testCase "an ambiguous XIRR is surfaced rather than silently swallowed" ambiguousXirr
        , testCase
            "asset-class configuration is the investment portfolio allowlist"
            assetClassAllowlist
        , testCase "settled accounts have complete empty summaries" settledAccount
        , testCase "monthly periods include months without transactions" contiguousMonthlyPeriods
        , investmentSummaryInvariantsTests
        , generatedBulkEquivalenceTests
        ]

retainedFdInterest :: IO ()
retainedFdInterest = do
    snapshotResult <- loadSnapshot "test/fixtures/investments.journal"
    mappings <-
        case mkInvestmentMappings [fdMapping] of
            Left message -> assertFailure (show message) >> fail "invalid mapping"
            Right value -> pure value
    case snapshotResult of
        Left loadError -> assertFailure (show loadError)
        Right snapshot -> do
            let summary =
                    investmentSummaryAt
                        snapshot
                        testAssetClasses
                        mappings
                        "INR"
                        (Just "assets:investments:fd:hdfc")
                        (fromGregorian 2026 3 31)
            assertEqual "market value" (Just 107000) summary.marketValue
            assertEqual "interest income" (Just 7000) summary.attributedIncome
            assertEqual "principal cost" (Just 100000) summary.purchaseCost
            assertEqual "unrealised gain" (Just 0) summary.gainLoss
            assertEqual "total profit" (Just 7000) summary.totalProfit
            assertAnnualRate "XIRR" summary.xirr 0.07 0.071
            assertEqual "completeness" Complete summary.completeness

fdMapping :: InvestmentMapping
fdMapping =
    InvestmentMapping
        { assetAccount = assetAccount "assets:investments:fd:hdfc"
        , performanceAccounts =
            [performanceAccount "income:investments:fd:interest" Interest]
        }

assetAccount :: Text.Text -> AssetAccount
assetAccount = either (error . Text.unpack) id . mkAssetAccount

performanceAccount :: Text.Text -> PerformanceRole -> PerformanceAccount
performanceAccount name role =
    PerformanceAccount{account = either (error . Text.unpack) id (mkAccountName name), role}

mappingWithoutPerformance :: Text.Text -> InvestmentMapping
mappingWithoutPerformance asset =
    InvestmentMapping{assetAccount = assetAccount asset, performanceAccounts = []}

descendantFdInterest :: IO ()
descendantFdInterest = do
    snapshotResult <- loadSnapshot "test/fixtures/investment-descendant.journal"
    mappings <-
        case mkInvestmentMappings [parentFdMapping] of
            Left message -> assertFailure (show message) >> fail "invalid mapping"
            Right value -> pure value
    case snapshotResult of
        Left loadError -> assertFailure (show loadError)
        Right snapshot -> do
            let summary =
                    investmentSummaryAt
                        snapshot
                        testAssetClasses
                        mappings
                        "INR"
                        (Just "assets:investments:fd:hdfc")
                        (fromGregorian 2026 3 31)
            assertEqual "market value" (Just 107000) summary.marketValue
            assertEqual "selected interest income" (Just 7000) summary.attributedIncome
            assertEqual "principal cost" (Just 100000) summary.purchaseCost
            assertEqual "total profit" (Just 7000) summary.totalProfit
            assertAnnualRate "XIRR" summary.xirr 0.07 0.071
            assertEqual "completeness" Complete summary.completeness

parentFdMapping :: InvestmentMapping
parentFdMapping =
    InvestmentMapping
        { assetAccount = assetAccount "assets:investments:fd"
        , performanceAccounts =
            [performanceAccount "income:investments:fd:interest" Interest]
        }

partialDisposal :: IO ()
partialDisposal = do
    snapshotResult <- loadSnapshot "test/fixtures/investments.journal"
    mappings <-
        case mkInvestmentMappings [equityMapping] of
            Left message -> assertFailure (show message) >> fail "invalid mapping"
            Right value -> pure value
    case snapshotResult of
        Left loadError -> assertFailure (show loadError)
        Right snapshot -> do
            let summary =
                    investmentSummaryAt
                        snapshot
                        testAssetClasses
                        mappings
                        "INR"
                        (Just "assets:investments:equity:xyz")
                        (fromGregorian 2026 3 31)
                flows =
                    externalCashFlowsAt
                        snapshot
                        testAssetClasses
                        mappings
                        "INR"
                        (Just "assets:investments:equity:xyz")
                        (fromGregorian 2026 3 31)
            assertEqual "remaining market value" (Just 900) summary.marketValue
            assertEqual "remaining cost" (Just 600) summary.purchaseCost
            assertEqual "unrealised gain" (Just 300) summary.gainLoss
            assertEqual "realised gain" (Just 200) summary.realisedGain
            assertEqual "total profit" (Just 500) summary.totalProfit
            assertEqual
                "XIRR uses actual purchase and sale cash"
                (Just [-1000, 600])
                (fmap (fmap cashFlowAmount) flows)

foreignAcquisitionCost :: IO ()
foreignAcquisitionCost = do
    snapshotResult <- loadSnapshot "test/fixtures/investments.journal"
    mappings <-
        case mkInvestmentMappings [mappingWithoutPerformance "assets:investments:equity"] of
            Left message -> assertFailure (show message) >> fail "invalid mapping"
            Right value -> pure value
    case snapshotResult of
        Left loadError -> assertFailure (show loadError)
        Right snapshot -> do
            let summary =
                    investmentSummaryAt
                        snapshot
                        testAssetClasses
                        mappings
                        "INR"
                        (Just "assets:investments:equity:foreign")
                        (fromGregorian 2026 3 31)
            assertEqual "market value" (Just 12750) summary.marketValue
            assertEqual "historical INR cost" (Just 8000) summary.purchaseCost
            assertEqual "unrealised gain" (Just 4750) summary.gainLoss
            assertEqual "total profit" (Just 4750) summary.totalProfit
            assertEqual "completeness inherited from parent" Complete summary.completeness

foreignCashHistoricalCost :: IO ()
foreignCashHistoricalCost = do
    snapshotResult <- loadSnapshot "test/fixtures/investment-foreign-cash.journal"
    mappings <-
        case mkInvestmentMappings [mappingWithoutPerformance "assets:cash:brokerage"] of
            Left message -> assertFailure (show message) >> fail "invalid mapping"
            Right value -> pure value
    case snapshotResult of
        Left loadError -> assertFailure (show loadError)
        Right snapshot -> do
            let summary =
                    investmentSummaryAt
                        snapshot
                        testAssetClasses
                        mappings
                        "INR"
                        (Just "assets:cash:brokerage")
                        (fromGregorian 2025 2 28)
            assertEqual
                "market value uses the latest monthly FX price"
                (Just 8200)
                summary.marketValue
            assertEqual "historical cost uses acquisition-date FX" (Just 8000) summary.purchaseCost
            assertEqual "foreign cash gain" (Just 200) summary.gainLoss
            assertEqual "completeness" Complete summary.completeness

externalCashFlows :: IO ()
externalCashFlows = do
    snapshotResult <- loadSnapshot "test/fixtures/investment-xirr.journal"
    mappings <-
        case mkInvestmentMappings [xirrMapping] of
            Left message -> assertFailure (show message) >> fail "invalid mapping"
            Right value -> pure value
    case snapshotResult of
        Left loadError -> assertFailure (show loadError)
        Right snapshot -> do
            let flows =
                    externalCashFlowsAt
                        snapshot
                        testAssetClasses
                        mappings
                        "INR"
                        (Just "assets:investments:fund")
                        (fromGregorian 2025 6 1)
            assertEqual
                "only investor cash movements remain"
                (Just [-100, 0, 0, 5, 0, -3])
                (fmap (fmap cashFlowAmount) flows)
            assertEqual
                "cash flows retain their transaction dates"
                (Just (fromGregorian 2025 1 1))
                (fmap (fmap cashFlowDate) flows >>= safeHead)

summaryXirr :: IO ()
summaryXirr = do
    snapshotResult <- loadSnapshot "test/fixtures/investment-xirr.journal"
    mappings <-
        case mkInvestmentMappings [xirrMapping] of
            Left message -> assertFailure (show message) >> fail "invalid mapping"
            Right value -> pure value
    case snapshotResult of
        Left loadError -> assertFailure (show loadError)
        Right snapshot -> do
            let summary =
                    investmentSummaryAt
                        snapshot
                        testAssetClasses
                        mappings
                        "INR"
                        (Just "assets:investments:fund")
                        (fromGregorian 2025 6 1)
            assertAnnualRate "summary XIRR" summary.xirr 0.2 0.3

ambiguousXirr :: IO ()
ambiguousXirr = do
    snapshotResult <- loadSnapshot "test/fixtures/investment-ambiguous-xirr.journal"
    mappings <-
        case mkInvestmentMappings [ambiguousMapping] of
            Left message -> assertFailure (show message) >> fail "invalid mapping"
            Right value -> pure value
    case snapshotResult of
        Left loadError -> assertFailure (show loadError)
        Right snapshot -> do
            let result =
                    summaryAt
                        snapshot
                        testAssetClasses
                        mappings
                        "INR"
                        (Just "assets:investments:ambiguous")
                        (fromGregorian 2026 1 1)
            assertEqual "ambiguous XIRR yields no rate" Nothing result.summary.xirr
            assertBool
                "an ambiguous XIRR must surface a warning or leave the summary incomplete"
                (not (null result.warnings) || result.summary.completeness /= Complete)

ambiguousMapping :: InvestmentMapping
ambiguousMapping =
    mappingWithoutPerformance "assets:investments:ambiguous"

assetClassAllowlist :: IO ()
assetClassAllowlist = do
    snapshotResult <- loadSnapshot "test/fixtures/investments.journal"
    assetClasses <-
        case mkAssetClassMappings
            [ AssetClassMapping
                Equity
                (assetAccount "assets:investments:equity:xyz" :| [])
                Nothing
            ] of
            Left message -> assertFailure (show message) >> pure emptyAssetClassMappings
            Right value -> pure value
    mappings <-
        case mkInvestmentMappings [equityMapping] of
            Left message -> assertFailure (show message) >> pure emptyInvestmentMappings
            Right value -> pure value
    case snapshotResult of
        Left loadError -> assertFailure (show loadError)
        Right snapshot -> do
            let summary =
                    investmentSummaryAt
                        snapshot
                        assetClasses
                        mappings
                        "INR"
                        Nothing
                        (fromGregorian 2026 3 31)
            assertEqual
                "only the configured account contributes market value"
                (Just 900)
                summary.marketValue
            assertEqual
                "only the configured account contributes purchase cost"
                (Just 600)
                summary.purchaseCost

settledAccount :: IO ()
settledAccount = do
    snapshotResult <- loadSnapshot "test/fixtures/investments.journal"
    case snapshotResult of
        Left loadError -> assertFailure (show loadError)
        Right snapshot ->
            assertEqual
                "settled account completeness"
                Complete
                ( investmentSummaryAt
                    snapshot
                    testAssetClasses
                    emptyInvestmentMappings
                    "INR"
                    (Just "assets:investments:equity:xyz")
                    (fromGregorian 2026 4 1)
                ).completeness

xirrMapping :: InvestmentMapping
xirrMapping =
    InvestmentMapping
        { assetAccount = assetAccount "assets:investments:fund"
        , performanceAccounts =
            [ performanceAccount "income:investments:fund:dividend" Interest
            , performanceAccount "expenses:investments:fund:fee" Fee
            ]
        }

assertAnnualRate :: String -> Maybe Decimal -> Decimal -> Decimal -> IO ()
assertAnnualRate label actual lower upper =
    case actual of
        Nothing -> assertFailure (label <> " was unavailable")
        Just value -> assertBool label (value > lower && value < upper)

safeHead :: [value] -> Maybe value
safeHead [] = Nothing
safeHead (value : _) = Just value

contiguousMonthlyPeriods :: IO ()
contiguousMonthlyPeriods =
    assertEqual
        "every month from the first transaction through the latest date"
        ["2025-01", "2025-02", "2025-03", "2025-04"]
        ( (\(key, _, _) -> key)
            <$> reportPeriods
                Monthly
                [transactionOn 2025 1 15, transactionOn 2025 4 2]
                (fromGregorian 2025 4 20)
        )

transactionOn :: Integer -> Int -> Int -> Hledger.Transaction
transactionOn year month day = nulltransaction{Hledger.tdate = fromGregorian year month day}

equityMapping :: InvestmentMapping
equityMapping =
    InvestmentMapping
        { assetAccount = assetAccount "assets:investments:equity:xyz"
        , performanceAccounts =
            [performanceAccount "income:investments:xyz:gains" RealisedGain]
        }

testAssetClasses :: AssetClassMappings
testAssetClasses =
    case mkAssetClassMappings
        [ AssetClassMapping Equity (assetAccount "assets:investments" :| []) Nothing
        , AssetClassMapping Cash (assetAccount "assets:cash:brokerage" :| []) Nothing
        ] of
        Left _ -> emptyAssetClassMappings
        Right value -> value

-- | Wide enough to cover every account touched by the edge-case journals.
edgeCaseAssetClasses :: AssetClassMappings
edgeCaseAssetClasses =
    case mkAssetClassMappings
        [ AssetClassMapping Equity (assetAccount "assets:investments" :| []) Nothing
        , AssetClassMapping Cash (assetAccount "assets:cash" :| []) Nothing
        ] of
        Left _ -> emptyAssetClassMappings
        Right value -> value

{- | (label, journal path, asset classes, investment mappings, scopes to check):
every fixture is checked at scope = Nothing plus at least one narrower scope.
Sampled by the investment summary invariants property.
-}
scopeScanFixtures ::
    [(String, Text.Text, AssetClassMappings, InvestmentMappings, [Maybe Text.Text])]
scopeScanFixtures =
    [
        ( "investments.journal"
        , "test/fixtures/investments.journal"
        , testAssetClasses
        , investmentsJournalMappings
        , [Nothing, Just "assets:investments:equity:xyz"]
        )
    ,
        ( "investment-descendant.journal"
        , "test/fixtures/investment-descendant.journal"
        , testAssetClasses
        , parentFdMappings
        , [Nothing, Just "assets:investments:fd:hdfc"]
        )
    ,
        ( "investment-foreign-cash.journal"
        , "test/fixtures/investment-foreign-cash.journal"
        , testAssetClasses
        , foreignCashMappings
        , [Nothing, Just "assets:cash:brokerage"]
        )
    ,
        ( "investment-xirr.journal"
        , "test/fixtures/investment-xirr.journal"
        , testAssetClasses
        , xirrJournalMappings
        , [Nothing, Just "assets:investments:fund:b"]
        )
    ,
        ( "investment-ambiguous-xirr.journal"
        , "test/fixtures/investment-ambiguous-xirr.journal"
        , testAssetClasses
        , ambiguousJournalMappings
        , [Nothing]
        )
    ,
        ( "investment-gapped-price.journal"
        , "test/fixtures/investment-gapped-price.journal"
        , edgeCaseAssetClasses
        , emptyInvestmentMappings
        , [Nothing]
        )
    ,
        ( "investment-never-priced.journal"
        , "test/fixtures/investment-never-priced.journal"
        , edgeCaseAssetClasses
        , emptyInvestmentMappings
        , [Nothing]
        )
    ,
        ( "investment-gain-fee-roles.journal"
        , "test/fixtures/investment-gain-fee-roles.journal"
        , edgeCaseAssetClasses
        , bug6Mappings
        , [Nothing, Just "assets:investments:equity"]
        )
    ,
        ( "investment-dust-commodity.journal"
        , "test/fixtures/investment-dust-commodity.journal"
        , edgeCaseAssetClasses
        , emptyInvestmentMappings
        , [Nothing]
        )
    ,
        ( "investment-nested-mapping.journal"
        , "test/fixtures/investment-nested-mapping.journal"
        , edgeCaseAssetClasses
        , nestedMappings
        , [Nothing, Just "assets:investments", Just "assets:investments:equity:xyz"]
        )
    ]

investmentsJournalMappings :: InvestmentMappings
investmentsJournalMappings = mappingsOrEmpty [fdMapping, equityMapping]

parentFdMappings :: InvestmentMappings
parentFdMappings = mappingsOrEmpty [parentFdMapping]

foreignCashMappings :: InvestmentMappings
foreignCashMappings = mappingsOrEmpty [InvestmentMapping (assetAccount "assets:cash:brokerage") []]

xirrJournalMappings :: InvestmentMappings
xirrJournalMappings = mappingsOrEmpty [xirrMapping]

ambiguousJournalMappings :: InvestmentMappings
ambiguousJournalMappings = mappingsOrEmpty [ambiguousMapping]

bug6Mappings :: InvestmentMappings
bug6Mappings =
    mappingsOrEmpty
        [ InvestmentMapping
            { assetAccount = assetAccount "assets:investments:equity"
            , performanceAccounts =
                [ performanceAccount "income:investments:xyz:gains" RealisedGain
                , performanceAccount "expenses:investments:fee" Fee
                ]
            }
        ]

{- | Parent and child both map to the same asset class, so a scope nested inside
either exercises 'mappedAssetScope' and 'transactionMatchesScope' together.
-}
nestedMappings :: InvestmentMappings
nestedMappings =
    mappingsOrEmpty
        [ InvestmentMapping
            { assetAccount = assetAccount "assets:investments"
            , performanceAccounts =
                [performanceAccount "income:investments:dividend" Interest]
            }
        ]

mappingsOrEmpty :: [InvestmentMapping] -> InvestmentMappings
mappingsOrEmpty mappings =
    case mkInvestmentMappings mappings of
        Left _ -> emptyInvestmentMappings
        Right value -> value

assertScopeScanMatchesDay ::
    InvestmentPerformanceContext ->
    Snapshot ->
    InvestmentMappings ->
    Maybe Text.Text ->
    Day ->
    ScopeScan ->
    IO ()
assertScopeScanMatchesDay context snapshot mappings scope day scan = do
    let assetClasses = context.assetClasses
        commodity = context.commodity
        expectedAccounts = investmentAccountsAt snapshot assetClasses scope day
        expectedMappingComplete =
            not (null expectedAccounts)
                && all (hasAttributablePerformance snapshot mappings day) expectedAccounts
        expectedRetained =
            retainedPerformanceAtWith
                context.valueAt
                snapshot
                assetClasses
                mappings
                commodity
                scope
                day
        expectedAttributed =
            attributedAtWith
                context.valueAt
                snapshot
                assetClasses
                mappings
                commodity
                scope
                day
        expectedCashFlows =
            externalCashFlowsAtWith
                context.valueAt
                snapshot
                assetClasses
                mappings
                commodity
                scope
                day
        label suffix = "day " <> show day <> " " <> suffix
    assertEqual (label "accounts") expectedAccounts scan.accounts
    assertEqual (label "mappingComplete") expectedMappingComplete scan.mappingComplete
    assertEqual (label "retained") expectedRetained scan.retained
    assertEqual (label "attributed") expectedAttributed scan.attributed
    assertEqual
        (label "cashFlows (multiset)")
        (sortCashFlows <$> expectedCashFlows)
        (sortCashFlows <$> scan.cashFlows)

sortCashFlows :: [CashFlow] -> [(Day, Decimal)]
sortCashFlows = List.sort . fmap (\flow -> (cashFlowDate flow, cashFlowAmount flow))

-- | Compares a per-day reference list with the bulk timeline, day by day.
assertDaySummariesMatch :: [Day] -> [SummaryResult] -> [SummaryResult] -> IO ()
assertDaySummariesMatch days expected actual = do
    assertEqual "same number of results" (length expected) (length actual)
    sequence_
        [ assertSummaryResultEqual ("day " <> show day) perDay bulk
        | (day, perDay, bulk) <- zip3 days expected actual
        ]

assertSummaryResultEqual :: String -> SummaryResult -> SummaryResult -> IO ()
assertSummaryResultEqual label expected actual = do
    assertSummaryEqual label expected.summary actual.summary
    assertEqual (label <> " warnings") expected.warnings actual.warnings

assertSummaryEqual :: String -> InvestmentSummary -> InvestmentSummary -> IO ()
assertSummaryEqual label expected actual = do
    assertEqual (label <> " marketValue") expected.marketValue actual.marketValue
    assertEqual (label <> " purchaseCost") expected.purchaseCost actual.purchaseCost
    assertEqual (label <> " gainLoss") expected.gainLoss actual.gainLoss
    assertEqual (label <> " xirr") expected.xirr actual.xirr
    assertEqual (label <> " attributedIncome") expected.attributedIncome actual.attributedIncome
    assertEqual (label <> " realisedGain") expected.realisedGain actual.realisedGain
    assertEqual (label <> " totalProfit") expected.totalProfit actual.totalProfit
    assertEqual (label <> " completeness") expected.completeness actual.completeness

{- | A scenario pairs one fixture and its configured asset classes and investment
mappings with one of that fixture's configured scopes. The fixture-matrix label
is display-only and unused here.
-}
genSummaryScenario :: Gen (Text.Text, AssetClassMappings, InvestmentMappings, Maybe Text.Text)
genSummaryScenario = do
    (_, path, assetClasses, mappings, scopes) <- Gen.element scopeScanFixtures
    scope <- Gen.element scopes
    pure (path, assetClasses, mappings, scope)

{- | The summary's own decomposition and completeness implications, sampled over
the existing fixture matrix rather than a hand-picked case list.
-}
investmentSummaryInvariantsTests :: TestTree
investmentSummaryInvariantsTests =
    testGroup
        "investment summary invariants"
        [ testProperty
            "summary decomposition recomposes with attribution and retained income"
            investmentSummaryDecomposition
        , testProperty
            "asset-class labels do not affect attribution"
            assetClassLabelsDoNotAffectSummary
        ]

investmentSummaryDecomposition :: Property
investmentSummaryDecomposition =
    property $ do
        (path, assetClasses, mappings, scope) <- forAll genSummaryScenario
        snapshot <- loadScenarioSnapshot path
        let day = fromMaybe (error "fixture has no journal date") snapshot.latestJournalDate
            context = prepareInvestmentPerformance snapshot assetClasses mappings "INR"
            result = summaryAtPrepared context scope day
            effectiveRetained =
                case scopeScansAt context scope [day] of
                    scan : _ ->
                        if scan.mappingComplete then scan.retained else Just 0
                    [] -> Nothing
            attribution =
                attributedAtWith
                    context.valueAt
                    snapshot
                    assetClasses
                    mappings
                    "INR"
                    scope
                    day
        footnote ("fixture " <> Text.unpack path <> " scope " <> show scope)
        assertTotalProfitDecomposition result.summary attribution
        assertCompletenessImplications result
        assertMarketValueGatesXirr result.summary
        assertMarketCostRetainedIdentity result.summary effectiveRetained

{- | Reclassifying an account from Equity to Debt (or Cash to Other) keeps the
same account prefixes, so attribution and valuation must not change. Only the
configured prefixes drive the summary; the asset-class label does not.
-}
assetClassLabelsDoNotAffectSummary :: Property
assetClassLabelsDoNotAffectSummary =
    property $ do
        snapshot <- forAll genInvestmentSnapshot
        scope <- forAll (Gen.element [Nothing, Just "assets:investments", Just "assets:property"])
        let day = latestGeneratedDate snapshot
            labelled =
                prepareInvestmentPerformance
                    snapshot
                    generatedAssetClasses
                    generatedMappings
                    displayCommodity
            relabelled =
                prepareInvestmentPerformance
                    snapshot
                    generatedAssetClassesRelabelled
                    generatedMappings
                    displayCommodity
        footnote ("scope " <> show scope <> ", latest " <> show day)
        liftIO $
            assertSummaryResultEqual
                "same prefixes, different asset-class label"
                (summaryAtPrepared labelled scope day)
                (summaryAtPrepared relabelled scope day)

loadScenarioSnapshot :: Text.Text -> PropertyT IO Snapshot
loadScenarioSnapshot path = do
    loaded <- liftIO (loadSnapshot path)
    case loaded of
        Left loadError -> do
            footnote (show loadError)
            failure
        Right snapshot -> pure snapshot

{- | Total profit is unrealised gain plus attributed income, realised gain, and
fees. The attribution triple is withheld when the mapping is incomplete, so a
missing triple leaves this guard unsatisfied.
-}
assertTotalProfitDecomposition ::
    InvestmentSummary -> Maybe (Decimal, Decimal, Decimal) -> PropertyT IO ()
assertTotalProfitDecomposition summary attribution =
    case (summary.totalProfit, summary.gainLoss, summary.attributedIncome, summary.realisedGain) of
        (Just total, Just gain, Just income, Just realised) ->
            total === gain + income + realised + fees
        _ -> pure ()
  where
    fees = maybe 0 (\(_, _, value) -> value) attribution

{- | Market value is purchase cost plus unrealised gain plus retained income.

The retained operand must be the summary's own *effective* retained: when the
mapping is incomplete the summary forces retained to zero and absorbs reinvested
income into the cost basis, while 'retainedPerformanceAtWith' would still report
it. Asserting the identity against the unforced retained only holds for complete
mappings.
-}
assertMarketCostRetainedIdentity :: InvestmentSummary -> Maybe Decimal -> PropertyT IO ()
assertMarketCostRetainedIdentity summary retained =
    case (summary.marketValue, summary.purchaseCost, summary.gainLoss, retained) of
        (Just market, Just cost, Just gain, Just retainedIncome) ->
            market - cost === gain + retainedIncome
        _ -> pure ()

{- | Completeness is a claim about the summary's own evidence: 'Complete' has no
warnings, 'Unavailable' has neither market value nor cost, and 'Partial' must
either warn or withhold the total profit.
-}
assertCompletenessImplications :: SummaryResult -> PropertyT IO ()
assertCompletenessImplications result = do
    when (result.summary.completeness == Complete) $
        assert (null result.warnings)
    when (result.summary.completeness == Unavailable) $
        assert (result.summary.marketValue == Nothing && result.summary.purchaseCost == Nothing)
    when (result.summary.completeness == Partial) $
        assert (not (null result.warnings) || result.summary.totalProfit == Nothing)

-- | Without a market value there is no terminal cash flow, so XIRR is undefined.
assertMarketValueGatesXirr :: InvestmentSummary -> PropertyT IO ()
assertMarketValueGatesXirr summary =
    when (summary.marketValue == Nothing) $
        assert (summary.xirr == Nothing)

{- | Asset classes for generated ledgers. 'genSnapshot' builds single-commodity
transactions against @assets:cash@, @assets:investments@, and @assets:property@,
so this matrix needs a Cash prefix that covers the generated cash accounts.
-}
generatedAssetClasses :: AssetClassMappings
generatedAssetClasses =
    either (error . Text.unpack) id $
        mkAssetClassMappings
            [ AssetClassMapping Equity (assetAccount "assets:investments" :| []) Nothing
            , AssetClassMapping Cash (assetAccount "assets:cash" :| []) Nothing
            , AssetClassMapping Property (assetAccount "assets:property" :| []) Nothing
            ]

{- | The same account prefixes as 'generatedAssetClasses' with different
asset-class labels. The label is an allowlist and presentation concern, so the
computed summary must be identical.
-}
generatedAssetClassesRelabelled :: AssetClassMappings
generatedAssetClassesRelabelled =
    either (error . Text.unpack) id $
        mkAssetClassMappings
            [ AssetClassMapping Debt (assetAccount "assets:investments" :| []) Nothing
            , AssetClassMapping Other (assetAccount "assets:cash" :| []) Nothing
            , AssetClassMapping Hybrid (assetAccount "assets:property" :| []) Nothing
            ]

{- | Investment mappings covering the generated performance accounts. The
property mapping deliberately has no performance accounts, matching the
generator's no-income property postings.
-}
generatedMappings :: InvestmentMappings
generatedMappings =
    either (error . Text.unpack) id $
        mkInvestmentMappings
            [ InvestmentMapping
                { assetAccount = assetAccount "assets:investments"
                , performanceAccounts =
                    [ performanceAccount "income:capital:gains" RealisedGain
                    , performanceAccount "income:salary" Interest
                    ]
                }
            , InvestmentMapping
                { assetAccount = assetAccount "assets:property"
                , performanceAccounts = []
                }
            ]

{- | Nonempty generated ledgers only: the bulk-equals-singular properties are
vacuous without at least one transaction the configured asset classes recognise.
-}
genInvestmentSnapshot :: Gen Snapshot
genInvestmentSnapshot = Gen.filter hasInvestments genSnapshot
  where
    hasInvestments snapshot =
        not (null (investmentDataTransactions snapshot generatedAssetClasses Nothing))

{- | The generated counterpart of the fixture matrices: over random valid
single-commodity ledgers, every bulk entry point must equal the singular per-day
or per-scope functions it replaces.
-}
generatedBulkEquivalenceTests :: TestTree
generatedBulkEquivalenceTests =
    testGroup
        "generated bulk equivalence"
        [ testProperty
            "holdings series equals per-day holdings"
            generatedHoldingsSeriesMatches
        , testProperty
            "summary timeline equals per-day summaries"
            generatedSummariesMatch
        , testProperty
            "scope scans equal per-day scope functions"
            generatedScopeScansMatch
        , testProperty
            "latest scope summaries equal singular"
            generatedScopesSummariesMatch
        ]

generatedHoldingsSeriesMatches :: Property
generatedHoldingsSeriesMatches =
    property $ do
        snapshot <- forAll genInvestmentSnapshot
        granularity <- forAll (Gen.element [Monthly, Yearly])
        scope <- forAll (Gen.element [Nothing, Just "assets:investments"])
        let days = generatedDays snapshot generatedAssetClasses scope granularity
            journal = snapshot.journal
            bulk =
                holdingsSeriesAtWithJournal
                    journal
                    generatedAssetClasses
                    scope
                    granularity
                    days
            perDay = fmap (holdingsAtWithJournal journal generatedAssetClasses scope) days
        footnoteGeneratedScenario snapshot scope granularity days
        bulk === perDay

generatedSummariesMatch :: Property
generatedSummariesMatch =
    property $ do
        snapshot <- forAll genInvestmentSnapshot
        granularity <- forAll (Gen.element [Monthly, Yearly])
        scope <- forAll (Gen.element [Nothing, Just "assets:investments"])
        let days = generatedDays snapshot generatedAssetClasses scope granularity
            context = generatedContext snapshot
            bulk = summariesAtPrepared context scope granularity days
            perDay = fmap (summaryAtPrepared context scope) days
        footnoteGeneratedScenario snapshot scope granularity days
        liftIO (assertDaySummariesMatch days perDay bulk)

generatedScopeScansMatch :: Property
generatedScopeScansMatch =
    property $ do
        snapshot <- forAll genInvestmentSnapshot
        granularity <- forAll (Gen.element [Monthly, Yearly])
        scope <- forAll (Gen.element [Nothing, Just "assets:investments"])
        let days = generatedDays snapshot generatedAssetClasses scope granularity
            context = generatedContext snapshot
            scans = scopeScansAt context scope days
        footnoteGeneratedScenario snapshot scope granularity days
        length scans === length days
        liftIO $
            sequence_
                [ assertScopeScanMatchesDay context snapshot generatedMappings scope day scan
                | (day, scan) <- zip days scans
                ]

generatedScopesSummariesMatch :: Property
generatedScopesSummariesMatch =
    property $ do
        snapshot <- forAll genInvestmentSnapshot
        let latest = latestGeneratedDate snapshot
            context = generatedContext snapshot
            scopes = [Nothing, Just "assets:investments", Just "assets:property", Nothing]
            bulk = summariesAtScopesPrepared context latest scopes
            singular = fmap (\scope -> summaryAtPrepared context scope latest) scopes
        footnote ("generated scopes: " <> show scopes)
        fmap fst bulk === scopes
        assert (null (summariesAtScopesPrepared context latest []))
        liftIO $
            sequence_
                [ assertSummaryResultEqual ("scope " <> show scope) expected actual
                | (scope, expected, (_, actual)) <- zip3 scopes singular bulk
                ]

generatedContext :: Snapshot -> InvestmentPerformanceContext
generatedContext snapshot =
    prepareInvestmentPerformance
        snapshot
        generatedAssetClasses
        generatedMappings
        displayCommodity

generatedDays ::
    Snapshot -> AssetClassMappings -> Maybe Text.Text -> Granularity -> [Day]
generatedDays snapshot assetClasses scope granularity =
    List.nub [end | (_, _, end) <- reportPeriods granularity transactions latest]
  where
    latest = latestGeneratedDate snapshot
    transactions = investmentDataTransactions snapshot assetClasses scope

latestGeneratedDate :: Snapshot -> Day
latestGeneratedDate snapshot =
    fromMaybe
        (error "generated snapshot has no journal date")
        snapshot.latestJournalDate

footnoteGeneratedScenario ::
    Snapshot -> Maybe Text.Text -> Granularity -> [Day] -> PropertyT IO ()
footnoteGeneratedScenario snapshot scope granularity days =
    footnote
        ( "generated ledger with "
            <> show (length snapshot.transactions)
            <> " transactions, latest "
            <> show snapshot.latestJournalDate
            <> ", scope "
            <> show scope
            <> ", "
            <> show granularity
            <> ", "
            <> show (length days)
            <> " days"
        )
