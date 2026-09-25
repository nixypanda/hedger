module Hedger.Domain.NetWorthSpec (
    netWorthSpec,
) where

import Control.Monad (when)
import Data.Foldable (for_)
import Data.List qualified as List
import Data.Maybe (isJust)
import Hedgehog (Gen, Property, failure, footnote, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hedger.Domain.NetWorth (
    NetWorthFigures (..),
    PerformancePoint (..),
    TimelinePoint (..),
    computeNetWorth,
 )
import Hedger.Domain.Period (Granularity (..))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Generators (displayCommodity, genSnapshot)
import Hedger.TestHelpers (sumQuantities)

netWorthSpec :: TestTree
netWorthSpec =
    testGroup
        "net worth"
        [ testGroup
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
                performanceMatchesTimeline
            ]
        ]

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

performanceRecomposes :: Property
performanceRecomposes =
    property $ do
        snapshot <- forAll genSnapshot
        granularity <- forAll genGranularity
        let figures = netWorthFigures snapshot granularity 2
        for_ figures.performance $ \point ->
            point.netInvestment + point.gainLoss === point.netWorth

performanceMatchesTimeline :: Property
performanceMatchesTimeline =
    property $ do
        snapshot <- forAll genSnapshot
        granularity <- forAll genGranularity
        let figures = netWorthFigures snapshot granularity 2
        for_ figures.performance $ \point ->
            case List.find ((== point.period) . (.period)) figures.timeline of
                Nothing -> footnote ("no timeline point for " <> show point.period) >> failure
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
