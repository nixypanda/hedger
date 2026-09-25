module Hedger.Domain.NetWorthSpec (
    netWorthSpec,
) where

import Control.Monad (when)
import Data.Foldable (for_)
import Data.List qualified as List
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Hedgehog (Gen, Property, failure, footnote, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

import Hedger.Domain.Internal.NetWorth (currentAssetCashBalance, currentLiabilityBalance)
import Hedger.Domain.NetWorth (
    NetWorthFigures (..),
    PerformancePoint (..),
    TimelinePoint (..),
    computeNetWorth,
 )
import Hedger.Domain.Period (Granularity (..))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (Conversion (..))
import Hedger.Generators (displayCommodity, genSnapshot)
import Hedger.Infrastructure.SnapshotStore (loadSnapshot)
import Hedger.TestHelpers (sumQuantities)

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
        , testGroup
            "net worth conservation"
            [ testProperty
                "each timeline point's net is assets minus liabilities"
                timelineNetIdentity
            , testProperty
                "current net worth is assets minus liabilities"
                currentNetWorthIdentity
            , testProperty
                "asset breakdown magnitude is invariant under display depth"
                assetsDepthInvariance
            , testProperty
                "liability breakdown magnitude is invariant under display depth"
                liabilitiesDepthInvariance
            ]
        , testGroup
            "performance decomposition"
            [ testProperty
                "each performance point's net worth is net investment plus gain/loss"
                performanceRecomposes
            , testProperty
                "each performance point has a matching timeline point"
                performanceAgreesWithTimeline
            ]
        ]
  where
    label journal granularity commodity =
        Text.unpack journal <> " " <> show granularity <> " " <> Text.unpack commodity

finalMonthlyPeriodUsesLatestJournalDate :: IO ()
finalMonthlyPeriodUsesLatestJournalDate = do
    snapshot <- loadFixture "test/fixtures/net-worth-future-price.journal"
    let figures = computeNetWorth snapshot snapshot.transactions Monthly "INR" Nothing Nothing 2 2
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
            computeNetWorth snapshot snapshot.transactions granularity commodity Nothing Nothing 2 2
        referenceAssets = currentAssetCashBalance snapshot snapshot.transactions commodity
        referenceLiabilities = currentLiabilityBalance snapshot snapshot.transactions commodity
    assertEqual "current assets" referenceAssets.value figures.currentAssets
    assertEqual "current liabilities" referenceLiabilities.value figures.currentLiabilities
    assertEqual
        "current net worth"
        ((-) <$> referenceAssets.value <*> referenceLiabilities.value)
        figures.currentNetWorth

loadFixture :: Text -> IO Snapshot
loadFixture path = loadSnapshot path >>= either (assertFailure . show) pure

depths :: [Int]
depths = [1, 2, 3, 4]

genGranularity :: Gen Granularity
genGranularity = Gen.element [Monthly, Yearly]

timelineNetIdentity :: Property
timelineNetIdentity =
    property $ do
        snapshot <- forAll genSnapshot
        granularity <- forAll genGranularity
        let figures = netWorthFigures snapshot granularity 2
        for_ figures.timeline $ \point ->
            point.net === point.positive - point.negative

currentNetWorthIdentity :: Property
currentNetWorthIdentity =
    property $ do
        snapshot <- forAll genSnapshot
        granularity <- forAll genGranularity
        let figures = netWorthFigures snapshot granularity 2
        when (isJust figures.currentAssets && isJust figures.currentLiabilities) $
            figures.currentNetWorth
                === ((-) <$> figures.currentAssets <*> figures.currentLiabilities)

assetsDepthInvariance :: Property
assetsDepthInvariance =
    property $ do
        snapshot <- forAll genSnapshot
        granularity <- forAll genGranularity
        let totalAt depth =
                sumQuantities
                    (netWorthFigures snapshot granularity depth).assetsBreakdownItems
        for_ depths $ \depth ->
            totalAt depth === totalAt 1

liabilitiesDepthInvariance :: Property
liabilitiesDepthInvariance =
    property $ do
        snapshot <- forAll genSnapshot
        granularity <- forAll genGranularity
        let totalAt depth =
                sumQuantities
                    (netWorthFigures snapshot granularity depth).liabilitiesBreakdownItems
        for_ depths $ \depth ->
            totalAt depth === totalAt 1

-- Performance decomposition

{- | Each performance column splits the period's net worth into the net
investment (contributions net of revaluation) and the gain/loss, so the two
components must add back to the reported net worth.
-}
performanceRecomposes :: Property
performanceRecomposes =
    property $ do
        snapshot <- forAll genSnapshot
        granularity <- forAll genGranularity
        let figures = netWorthFigures snapshot granularity 2
        for_ figures.performance $ \point ->
            point.netInvestment + point.gainLoss === point.netWorth

{- | Performance and timeline are projected from the same periodic columns and
filtered by the same data periods, so every performance point's period must
appear in the timeline with the same net worth.
-}
performanceAgreesWithTimeline :: Property
performanceAgreesWithTimeline =
    property $ do
        snapshot <- forAll genSnapshot
        granularity <- forAll genGranularity
        let figures = netWorthFigures snapshot granularity 2
        for_ figures.performance $ \point ->
            case List.find (\candidate -> candidate.period == point.period) figures.timeline of
                Nothing -> do
                    footnote
                        ( "performance period "
                            <> Text.unpack point.period
                            <> " is absent from the timeline"
                        )
                    failure
                Just timelinePoint -> point.netWorth === timelinePoint.net

netWorthFigures :: Snapshot -> Granularity -> Int -> NetWorthFigures
netWorthFigures snapshot granularity depth =
    computeNetWorth
        snapshot
        snapshot.transactions
        granularity
        displayCommodity
        Nothing
        Nothing
        depth
        depth
