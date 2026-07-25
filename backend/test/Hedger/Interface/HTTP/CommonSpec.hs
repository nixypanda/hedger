module Hedger.Interface.HTTP.CommonSpec (commonSpec) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Types qualified as Hledger

import Hedger.Interface.HTTP.Common (CommodityQuantity (..), commodityQuantities)

commonSpec :: TestTree
commonSpec =
    testGroup
        "HTTP common projections"
        [ testCase "aggregates priced and unpriced lots sharing a commodity" aggregatesSameCommodity
        ]

aggregatesSameCommodity :: IO ()
aggregatesSameCommodity =
    assertEqual
        "commodity quantities"
        [CommodityQuantity{commodity = "AAPL", quantity = "5"}]
        (commodityQuantities sameCommodityLots)

sameCommodityLots :: Hledger.MixedAmount
sameCommodityLots =
    HledgerAmount.mixed
        [ commodityAmount "AAPL" 2
        , commodityAmount "AAPL" 3 HledgerAmount.@@ commodityAmount "INR" 300
        ]

commodityAmount :: Hledger.CommoditySymbol -> Hledger.Quantity -> Hledger.Amount
commodityAmount commodity =
    HledgerAmount.amountWithCommodity commodity . HledgerAmount.num
