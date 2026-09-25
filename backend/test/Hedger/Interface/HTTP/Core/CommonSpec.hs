module Hedger.Interface.HTTP.Core.CommonSpec (
    commonSpec,
) where

import Data.Aeson (Value (..), toJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Error (AppError (..))
import Hedger.Domain.Period (Granularity (Monthly))
import Hedger.Interface.HTTP.Core.Common (
    CommodityQuantity (..),
    commodityQuantities,
    parseGranularity,
 )
import Hedger.Interface.HTTP.Core.Errors (toEnvelope)

commonSpec :: TestTree
commonSpec =
    testGroup
        "HTTP common projections"
        [ testCase "aggregates priced and unpriced lots sharing a commodity" aggregatesSameCommodity
        , testCase "parses granularity case-insensitively" $
            assertEqual "" (Right Monthly) (parseGranularity "MONTHLY")
        , testCase
            "error envelope serializes its payload under the error key"
            errorEnvelopeUsesErrorKey
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
        , commodityAmount "AAPL" 3
            HledgerAmount.@@ commodityAmount "INR" 300
        ]

commodityAmount :: Hledger.CommoditySymbol -> Hledger.Quantity -> Hledger.Amount
commodityAmount commodity = HledgerAmount.amountWithCommodity commodity . HledgerAmount.num

errorEnvelopeUsesErrorKey :: IO ()
errorEnvelopeUsesErrorKey =
    case toJSON (toEnvelope "req-123" (InvalidRequest "bad")) of
        Object envelopeObject -> do
            assertBool "envelope should expose the error under \"error\"" $
                KeyMap.member "error" envelopeObject
            assertBool "envelope should not expose the error under \"errorPayload\"" $
                not (KeyMap.member "errorPayload" envelopeObject)
            case KeyMap.lookup "error" envelopeObject of
                Just (Object payloadObject) -> do
                    assertEqual
                        "error payload should have a code"
                        True
                        $ KeyMap.member "code" payloadObject
                    assertEqual "error payload should have a message" True $
                        KeyMap.member "message" payloadObject
                _ -> assertFailure "expected \"error\" to hold a JSON object"
        _ -> assertFailure "expected the envelope to encode as a JSON object"
