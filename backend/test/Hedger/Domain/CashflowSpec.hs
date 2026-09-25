module Hedger.Domain.CashflowSpec (
    cashflowSpec,
) where

import Data.Foldable (for_)
import Hedgehog (Gen, Property, assert, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hedger.Domain.Cashflow (
    CashflowFigures (..),
    TimelinePoint (..),
    computeCashflow,
 )
import Hedger.Domain.Period (Granularity (..))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Generators (displayCommodity, genSnapshot)
import Hedger.TestHelpers (sumQuantities)

cashflowSpec :: TestTree
cashflowSpec =
    testGroup
        "cashflow"
        [ testGroup
            "cashflow conservation"
            [ testProperty
                "available figures have non-negative sides"
                nonNegativeSides
            , testProperty
                "income breakdown magnitude is invariant under display depth"
                incomeDepthInvariance
            , testProperty
                "expense breakdown magnitude is invariant under display depth"
                expenseDepthInvariance
            ]
        ]

depths :: [Int]
depths = [1, 2, 3, 4]

genGranularity :: Gen Granularity
genGranularity = Gen.element [Monthly, Yearly]

nonNegativeSides :: Property
nonNegativeSides =
    property $ do
        snapshot <- forAll genSnapshot
        granularity <- forAll genGranularity
        let figures = cashflowFigures snapshot granularity 1
        if figures.available
            then for_ figures.timeline $ \point ->
                assert (point.positive >= 0 && point.negative >= 0)
            else pure ()

incomeDepthInvariance :: Property
incomeDepthInvariance =
    property $ do
        snapshot <- forAll genSnapshot
        granularity <- forAll genGranularity
        let totalAt depth =
                sumQuantities
                    (cashflowFigures snapshot granularity depth).incomeBreakdownItems
        for_ depths $ \depth ->
            totalAt depth === totalAt 1

expenseDepthInvariance :: Property
expenseDepthInvariance =
    property $ do
        snapshot <- forAll genSnapshot
        granularity <- forAll genGranularity
        let totalAt depth =
                sumQuantities
                    (cashflowFigures snapshot granularity depth).expenseBreakdownItems
        for_ depths $ \depth ->
            totalAt depth === totalAt 1

cashflowFigures :: Snapshot -> Granularity -> Int -> CashflowFigures
cashflowFigures snapshot granularity depth =
    computeCashflow
        snapshot
        snapshot.transactions
        granularity
        displayCommodity
        Nothing
        Nothing
        depth
        depth
