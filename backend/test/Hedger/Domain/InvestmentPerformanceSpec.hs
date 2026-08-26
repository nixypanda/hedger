module Hedger.Domain.InvestmentPerformanceSpec (investmentsSpec) where

import Data.Decimal (Decimal)
import Data.Either (isLeft, isRight)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Time (fromGregorian)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Hledger.Data.Transaction (nulltransaction)
import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Internal.InvestmentPerformance (
    holdingsAtWithJournal,
    holdingsSeriesAtWithJournal,
 )
import Hedger.Domain.Investment (
    AssetClass (Cash, Equity),
    AssetClassMapping (..),
    AssetClassMappings,
    InvestmentMapping (..),
    PerformanceAccount (..),
    PerformanceRole (Fee, Interest, RealisedGain),
    emptyAssetClassMappings,
    emptyInvestmentMappings,
    mkAssetClassMappings,
    mkInvestmentMappings,
 )
import Hedger.Domain.InvestmentPerformance (
    InvestmentCompleteness (Complete),
    InvestmentSummary (..),
    SummaryResult (..),
    externalCashFlowsAt,
    investmentDataTransactions,
    investmentSummaryAt,
    reportPeriods,
    summaryAt,
 )
import Hedger.Domain.Period (Granularity (Monthly, Yearly))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Xirr (cashFlowAmount, cashFlowDate)
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
            "mapping validation rejects overlapping performance ownership but permits no-income assets"
            mappingValidation
        , testCase
            "XIRR cash flows cancel reinvestment and transfers while retaining external income and fees"
            externalCashFlows
        , testCase "investment summary reports annualised XIRR instead of absolute return" summaryXirr
        , testCase "an ambiguous XIRR is surfaced rather than silently swallowed" ambiguousXirr
        , testCase "asset-class configuration is the investment portfolio allowlist" assetClassAllowlist
        , testCase "settled accounts have complete empty summaries" settledAccount
        , testCase "monthly periods include months without transactions" contiguousMonthlyPeriods
        , holdingsSeriesRegressionTests
        ]

retainedFdInterest :: IO ()
retainedFdInterest = do
    snapshotResult <- loadSnapshot "test/fixtures/investments.journal"
    mappings <- case mkInvestmentMappings [fdMapping] of
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
        { assetAccount = "assets:investments:fd:hdfc"
        , performanceAccounts =
            [ PerformanceAccount
                { account = "income:investments:fd:interest"
                , role = Interest
                }
            ]
        }

descendantFdInterest :: IO ()
descendantFdInterest = do
    snapshotResult <- loadSnapshot "test/fixtures/investment-descendant.journal"
    mappings <- case mkInvestmentMappings [parentFdMapping] of
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
        { assetAccount = "assets:investments:fd"
        , performanceAccounts =
            [ PerformanceAccount
                { account = "income:investments:fd:interest"
                , role = Interest
                }
            ]
        }

partialDisposal :: IO ()
partialDisposal = do
    snapshotResult <- loadSnapshot "test/fixtures/investments.journal"
    mappings <- case mkInvestmentMappings [equityMapping] of
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
    mappings <- case mkInvestmentMappings [InvestmentMapping "assets:investments:equity" []] of
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
    mappings <- case mkInvestmentMappings [InvestmentMapping "assets:cash:brokerage" []] of
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
            assertEqual "market value uses the latest monthly FX price" (Just 8200) summary.marketValue
            assertEqual "historical cost uses acquisition-date FX" (Just 8000) summary.purchaseCost
            assertEqual "foreign cash gain" (Just 200) summary.gainLoss
            assertEqual "completeness" Complete summary.completeness

mappingValidation :: IO ()
mappingValidation = do
    let emptyProperty = InvestmentMapping "assets:property:home" []
        first =
            InvestmentMapping
                "assets:investments:a"
                [PerformanceAccount "income:investments:shared" Interest]
        second =
            InvestmentMapping
                "assets:investments:b"
                [PerformanceAccount "income:investments:shared:detail" Interest]
    assertBool
        "an explicit no-income property mapping is valid"
        (isRight (mkInvestmentMappings [emptyProperty]))
    assertBool "overlapping performance scopes are rejected" (isLeft (mkInvestmentMappings [first, second]))

externalCashFlows :: IO ()
externalCashFlows = do
    snapshotResult <- loadSnapshot "test/fixtures/investment-xirr.journal"
    mappings <- case mkInvestmentMappings [xirrMapping] of
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
    mappings <- case mkInvestmentMappings [xirrMapping] of
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
    mappings <- case mkInvestmentMappings [ambiguousMapping] of
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
    InvestmentMapping
        { assetAccount = "assets:investments:ambiguous"
        , performanceAccounts = []
        }

assetClassAllowlist :: IO ()
assetClassAllowlist = do
    snapshotResult <- loadSnapshot "test/fixtures/investments.journal"
    assetClasses <-
        case mkAssetClassMappings
            [AssetClassMapping Equity ["assets:investments:equity:xyz"] Nothing] of
            Left message -> assertFailure (show message) >> pure emptyAssetClassMappings
            Right value -> pure value
    mappings <- case mkInvestmentMappings [equityMapping] of
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
            assertEqual "only the configured account contributes market value" (Just 900) summary.marketValue
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
        { assetAccount = "assets:investments:fund"
        , performanceAccounts =
            [ PerformanceAccount "income:investments:fund:dividend" Interest
            , PerformanceAccount "expenses:investments:fund:fee" Fee
            ]
        }

assertAnnualRate :: String -> Maybe Decimal -> Decimal -> Decimal -> IO ()
assertAnnualRate label actual lower upper = case actual of
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
                [ transactionOn 2025 1 15
                , transactionOn 2025 4 2
                ]
                (fromGregorian 2025 4 20)
        )

transactionOn :: Integer -> Int -> Int -> Hledger.Transaction
transactionOn year month day =
    nulltransaction{Hledger.tdate = fromGregorian year month day}

equityMapping :: InvestmentMapping
equityMapping =
    InvestmentMapping
        { assetAccount = "assets:investments:equity:xyz"
        , performanceAccounts = [PerformanceAccount "income:investments:xyz:gains" RealisedGain]
        }

testAssetClasses :: AssetClassMappings
testAssetClasses =
    case mkAssetClassMappings
        [ AssetClassMapping Equity ["assets:investments"] Nothing
        , AssetClassMapping Cash ["assets:cash:brokerage"] Nothing
        ] of
        Left _ -> emptyAssetClassMappings
        Right value -> value

-- | Wide enough to cover every account touched by the edge-case journals.
edgeCaseAssetClasses :: AssetClassMappings
edgeCaseAssetClasses =
    case mkAssetClassMappings
        [ AssetClassMapping Equity ["assets:investments"] Nothing
        , AssetClassMapping Cash ["assets:cash"] Nothing
        ] of
        Left _ -> emptyAssetClassMappings
        Right value -> value

holdingsSeriesRegressionTests :: TestTree
holdingsSeriesRegressionTests =
    testGroup
        "holdingsSeriesAtWithJournal agrees with holdingsAtWithJournal"
        (concatMap fixtureCases fixtures)
  where
    fixtures =
        [
            ( "investments.journal"
            , "test/fixtures/investments.journal"
            , testAssetClasses
            , [Nothing, Just "assets:investments:equity:xyz"]
            )
        ,
            ( "investment-descendant.journal"
            , "test/fixtures/investment-descendant.journal"
            , testAssetClasses
            , [Nothing]
            )
        ,
            ( "investment-foreign-cash.journal"
            , "test/fixtures/investment-foreign-cash.journal"
            , testAssetClasses
            , [Nothing, Just "assets:cash:brokerage"]
            )
        ,
            ( "investment-xirr.journal"
            , "test/fixtures/investment-xirr.journal"
            , testAssetClasses
            , [Nothing]
            )
        ,
            ( "investment-ambiguous-xirr.journal"
            , "test/fixtures/investment-ambiguous-xirr.journal"
            , testAssetClasses
            , [Nothing]
            )
        ,
            ( "bug1-gapped-price.journal"
            , "../docs/edge-case-journals/bug1-gapped-price.journal"
            , edgeCaseAssetClasses
            , [Nothing]
            )
        ,
            ( "bug1-never-priced.journal"
            , "../docs/edge-case-journals/bug1-never-priced.journal"
            , edgeCaseAssetClasses
            , [Nothing]
            )
        ,
            ( "bug6-gain-fee-roles.journal"
            , "../docs/edge-case-journals/bug6-gain-fee-roles.journal"
            , edgeCaseAssetClasses
            , [Nothing, Just "assets:investments:equity"]
            )
        ,
            ( "bug7-ambiguous-xirr.journal"
            , "../docs/edge-case-journals/bug7-ambiguous-xirr.journal"
            , edgeCaseAssetClasses
            , [Nothing]
            )
        ,
            ( "bug8-dust-commodity.journal"
            , "../docs/edge-case-journals/bug8-dust-commodity.journal"
            , edgeCaseAssetClasses
            , [Nothing]
            )
        ,
            ( "lowsev-nested-mapping-retained.journal"
            , "../docs/edge-case-journals/lowsev-nested-mapping-retained.journal"
            , edgeCaseAssetClasses
            , [Nothing, Just "assets:investments"]
            )
        ]
    fixtureCases (label, path, assetClasses, scopes) =
        [ testCase
            (label <> " " <> show granularity <> " scope=" <> show scope)
            (assertHoldingsSeriesMatches path assetClasses scope granularity)
        | granularity <- [Monthly, Yearly]
        , scope <- scopes
        ]

assertHoldingsSeriesMatches :: Text.Text -> AssetClassMappings -> Maybe Text.Text -> Granularity -> IO ()
assertHoldingsSeriesMatches path assetClasses scope granularity = do
    snapshotResult <- loadSnapshot path
    case snapshotResult of
        Left loadError -> assertFailure (show loadError)
        Right snapshot -> do
            let latest = fromMaybe (error "fixture has no journal date") snapshot.latestJournalDate
                transactions = investmentDataTransactions snapshot assetClasses scope
                days = [end | (_, _, end) <- reportPeriods granularity transactions latest]
                journal = snapshot.journal
                bulk = holdingsSeriesAtWithJournal journal assetClasses scope granularity days
                perDate = fmap (holdingsAtWithJournal journal assetClasses scope) days
            assertEqual "bulk holdings series matches per-date holdings" perDate bulk
