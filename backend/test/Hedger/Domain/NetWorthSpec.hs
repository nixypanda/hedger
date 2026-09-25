module Hedger.Domain.NetWorthSpec (
    netWorthSpec,
) where

import Data.Foldable (for_)
import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hedger.Domain.NetWorth (
    NetWorthFigures (..),
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
                "asset breakdown magnitude is invariant under display depth"
                assetsDepthInvariance
            , testProperty
                "liability breakdown magnitude is invariant under display depth"
                liabilitiesDepthInvariance
            ]
        ]

depths :: [Int]
depths = [1, 2, 3, 4]

genGranularity :: Gen Granularity
genGranularity = Gen.element [Monthly, Yearly]

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
