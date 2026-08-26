module Hedger.Domain.NetWorthSpec (netWorthSpec) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

import Hedger.Domain.NetWorth (NetWorthFigures (..), TimelinePoint (..), computeNetWorth)
import Hedger.Domain.Period (Granularity (Monthly))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.SnapshotStore (loadSnapshot)

netWorthSpec :: TestTree
netWorthSpec =
    testGroup
        "net worth"
        [ testCase
            "values the final monthly period at the latest journal date"
            finalMonthlyPeriodUsesLatestJournalDate
        ]

finalMonthlyPeriodUsesLatestJournalDate :: IO ()
finalMonthlyPeriodUsesLatestJournalDate = do
    snapshot <-
        loadSnapshot "test/fixtures/net-worth-future-price.journal"
            >>= either (assertFailure . show) pure
    let figures = computeNetWorth snapshot snapshot.transactions Monthly "INR" Nothing Nothing 2 2
    assertEqual "strict current assets" (Just 100) figures.currentAssets
    case figures.timeline of
        [point] -> do
            assertEqual "period" "2024-04" point.period
            assertEqual "historical assets" 100 point.positive
        points -> assertFailure ("expected one timeline point, got " <> show (length points))
