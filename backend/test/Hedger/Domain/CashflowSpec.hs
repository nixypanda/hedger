module Hedger.Domain.CashflowSpec (
    cashflowSpec,
) where

import Data.Foldable (for_)
import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hedger.Domain.Account (DisplayDepth (..))
import Hedger.Domain.Cashflow (
    CashflowFigures (..),
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
                "income breakdown magnitude is invariant under display depth"
                incomeDepthInvariance
            , testProperty
                "expense breakdown magnitude is invariant under display depth"
                expenseDepthInvariance
            ]
        ]

depths :: [DisplayDepth]
depths = [Depth1, Depth2, Depth3, Depth4]

genGranularity :: Gen Granularity
genGranularity = Gen.element [Monthly, Yearly]

incomeDepthInvariance :: Property
incomeDepthInvariance =
    property $ do
        snapshot <- forAll genSnapshot
        granularity <- forAll genGranularity
        let totalAt depth =
                sumQuantities
                    (cashflowFigures snapshot granularity depth).incomeBreakdownItems
        for_ depths $ \depth ->
            totalAt depth === totalAt Depth1

expenseDepthInvariance :: Property
expenseDepthInvariance =
    property $ do
        snapshot <- forAll genSnapshot
        granularity <- forAll genGranularity
        let totalAt depth =
                sumQuantities
                    (cashflowFigures snapshot granularity depth).expenseBreakdownItems
        for_ depths $ \depth ->
            totalAt depth === totalAt Depth1

cashflowFigures :: Snapshot -> Granularity -> DisplayDepth -> CashflowFigures
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
