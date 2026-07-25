module Hedger.Application.InvestmentsSpec (investmentsSpec) where

import Data.Either (isLeft, isRight)
import Data.Time (fromGregorian)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Hedger.Application.Investments (
    InvestmentCompleteness (Complete),
    InvestmentSummary (..),
    investmentSummaryAt,
 )
import Hedger.Domain.Investment (
    InvestmentMapping (..),
    PerformanceAccount (..),
    PerformanceRole (Interest, RealisedGain),
    mkInvestmentMappings,
 )
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
                        mappings
                        "INR"
                        (Just "assets:investments:fd:hdfc")
                        (fromGregorian 2026 3 31)
            assertEqual "market value" (Just 107000) summary.marketValue
            assertEqual "interest income" (Just 7000) summary.attributedIncome
            assertEqual "principal cost" (Just 100000) summary.purchaseCost
            assertEqual "unrealised gain" (Just 0) summary.gainLoss
            assertEqual "total profit" (Just 7000) summary.totalProfit
            assertEqual "return" (Just 7) summary.returnPercentage
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
                        mappings
                        "INR"
                        (Just "assets:investments:fd:hdfc")
                        (fromGregorian 2026 3 31)
            assertEqual "market value" (Just 107000) summary.marketValue
            assertEqual "selected interest income" (Just 7000) summary.attributedIncome
            assertEqual "principal cost" (Just 100000) summary.purchaseCost
            assertEqual "total profit" (Just 7000) summary.totalProfit
            assertEqual "return" (Just 7) summary.returnPercentage
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
                        mappings
                        "INR"
                        (Just "assets:investments:equity:xyz")
                        (fromGregorian 2026 3 31)
            assertEqual "remaining market value" (Just 900) summary.marketValue
            assertEqual "remaining cost" (Just 600) summary.purchaseCost
            assertEqual "unrealised gain" (Just 300) summary.gainLoss
            assertEqual "realised gain" (Just 200) summary.realisedGain
            assertEqual "total profit" (Just 500) summary.totalProfit

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

equityMapping :: InvestmentMapping
equityMapping =
    InvestmentMapping
        { assetAccount = "assets:investments:equity:xyz"
        , performanceAccounts = [PerformanceAccount "income:investments:xyz:gains" RealisedGain]
        }
