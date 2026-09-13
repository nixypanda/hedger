module Hedger.Domain.NetWorthSpec (netWorthSpec) where

import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

import Hedger.Domain.Internal.NetWorth (currentAssetCashBalance, currentLiabilityBalance)
import Hedger.Domain.NetWorth (NetWorthFigures (..), TimelinePoint (..), computeNetWorth)
import Hedger.Domain.Period (Granularity (..), ReportWindow, allPeriods, latestPeriods)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (Conversion (..))
import Hedger.Infrastructure.SnapshotStore (loadSnapshot)

netWorthSpec :: TestTree
netWorthSpec =
    testGroup
        "net worth"
        [ testCase
            "values the final monthly period at the latest journal date"
            finalMonthlyPeriodUsesLatestJournalDate
        , testGroup
            "current KPIs agree with the standalone strict balance reports"
            [ testCase (label journal granularity commodity) $
                currentAgreesWithReference journal granularity commodity
            | journal <- referenceJournals
            , granularity <- [Monthly, Yearly]
            , commodity <- ["INR", "USD"]
            ]
        ]
  where
    label journal granularity commodity =
        Text.unpack journal <> " " <> show granularity <> " " <> Text.unpack commodity

finalMonthlyPeriodUsesLatestJournalDate :: IO ()
finalMonthlyPeriodUsesLatestJournalDate = do
    snapshot <- loadFixture "test/fixtures/net-worth-future-price.journal"
    let figures =
            computeNetWorth
                snapshot
                snapshot.transactions
                Monthly
                (latestPeriods 24)
                "INR"
                Nothing
                Nothing
                2
                2
    assertEqual "strict current assets" (Just 100) figures.currentAssets
    case figures.timeline of
        [point] -> do
            assertEqual "period" "2024-04" point.period
            assertEqual "historical assets" 100 point.positive
        points -> assertFailure ("expected one timeline point, got " <> show (length points))

{- | The strict current KPIs are now projected from the final periodic column
rather than from their own pair of balance reports. These journals cover missing
prices, foreign cost, and gapped price histories, which are the cases where the
completeness rule and the valuation date could diverge.
-}
referenceJournals :: [Text]
referenceJournals =
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

currentAgreesWithReference :: Text -> Granularity -> Text -> IO ()
currentAgreesWithReference path granularity commodity = do
    snapshot <- loadFixture path
    let figures =
            computeNetWorth
                snapshot
                snapshot.transactions
                granularity
                (windowFor granularity)
                commodity
                Nothing
                Nothing
                2
                2
        referenceAssets = currentAssetCashBalance snapshot snapshot.transactions commodity
        referenceLiabilities = currentLiabilityBalance snapshot snapshot.transactions commodity
    assertEqual "current assets" referenceAssets.value figures.currentAssets
    assertEqual "current liabilities" referenceLiabilities.value figures.currentLiabilities
    assertEqual
        "current net worth"
        ((-) <$> referenceAssets.value <*> referenceLiabilities.value)
        figures.currentNetWorth

windowFor :: Granularity -> ReportWindow
windowFor Monthly = latestPeriods 24
windowFor Yearly = allPeriods

loadFixture :: Text -> IO Snapshot
loadFixture path = loadSnapshot path >>= either (assertFailure . show) pure
