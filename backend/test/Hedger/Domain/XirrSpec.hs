module Hedger.Domain.XirrSpec (xirrSpec) where

import Data.Decimal (Decimal, realFracToDecimal)
import Data.Time (Day (ModifiedJulianDay), addDays)
import Hedgehog (Gen, Property, PropertyT, assert, failure, footnote, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.Hedgehog (testProperty)

import Hedger.Domain.Xirr (
    AnnualRate,
    CashFlow,
    XirrError (..),
    annualRate,
    cashFlow,
    cashFlowAmount,
    cashFlowDate,
    xirr,
 )

xirrSpec :: TestTree
xirrSpec =
    testGroup
        "XIRR"
        [ testCase "calculates a known one-year return" knownOneYearReturn
        , testCase "calculates irregular dated contributions" irregularContributions
        , testCase "rejects same-sign cash flows" rejectsSameSignCashFlows
        , testCase "reports multiple roots as ambiguous" reportsAmbiguousRoots
        , testProperty "positive scaling preserves XIRR" positiveScalingPreservesXirr
        , testProperty "uniformly shifting dates preserves XIRR" shiftingDatesPreservesXirr
        ]

knownOneYearReturn :: IO ()
knownOneYearReturn =
    assertRateApprox 0.1 (xirr [cashFlow origin (-100), cashFlow (addDays 365 origin) 110])

irregularContributions :: IO ()
irregularContributions =
    assertRateApprox
        0.2425
        ( xirr
            [ cashFlow origin (-1000)
            , cashFlow (addDays 182 origin) (-500)
            , cashFlow (addDays 365 origin) 1800
            ]
        )

rejectsSameSignCashFlows :: IO ()
rejectsSameSignCashFlows =
    assertEqual
        "same-sign flows"
        (Left CashFlowsDoNotChangeSign)
        (xirr [cashFlow origin (-100), cashFlow (addDays 365 origin) (-110)])

reportsAmbiguousRoots :: IO ()
reportsAmbiguousRoots =
    assertEqual
        "multiple rates"
        (Left AmbiguousRates)
        ( xirr
            [ cashFlow origin (-100)
            , cashFlow (addDays 365 origin) 230
            , cashFlow (addDays 730 origin) (-132)
            ]
        )

assertRateApprox :: Double -> Either XirrError AnnualRate -> IO ()
assertRateApprox expected result = case result of
    Left problem -> assertBool ("unexpected error: " <> show problem) False
    Right rate ->
        assertBool
            ("expected " <> show expected <> ", got " <> show (decimalDouble (annualRate rate)))
            (abs (decimalDouble (annualRate rate) - expected) < 0.002)

positiveScalingPreservesXirr :: Property
positiveScalingPreservesXirr = property $ do
    flows <- forAll genCashFlows
    scale <- forAll (Gen.integral (Range.linear 1 100) :: Gen Integer)
    assertSameRate (xirr flows) (xirr (scaleCashFlows scale flows))

shiftingDatesPreservesXirr :: Property
shiftingDatesPreservesXirr = property $ do
    flows <- forAll genCashFlows
    offset <- forAll (Gen.integral (Range.linear 1 10000) :: Gen Integer)
    assertSameRate (xirr flows) (xirr (shiftCashFlows offset flows))

assertSameRate :: Either XirrError AnnualRate -> Either XirrError AnnualRate -> PropertyT IO ()
assertSameRate left right = case (left, right) of
    (Right first, Right second) ->
        assert (abs (decimalDouble (annualRate first) - decimalDouble (annualRate second)) < 0.0000001)
    _ -> do
        footnote ("first result: " <> show left)
        footnote ("second result: " <> show right)
        failure

genCashFlows :: Gen [CashFlow]
genCashFlows = do
    start <- ModifiedJulianDay <$> Gen.integral (Range.linear 40000 70000)
    principal <- Gen.integral (Range.linear 100 10000)
    contribution <- Gen.integral (Range.linear 0 5000)
    contributionDay <- Gen.integral (Range.linear 1 364)
    terminalDay <- Gen.integral (Range.linear 365 3650)
    multiplier <- Gen.double (Range.linearFrac 0.6 2.0)
    let terminal = realFracToDecimal 6 (fromIntegral (principal + contribution) * multiplier :: Double)
        intermediate =
            [ cashFlow (addDays contributionDay start) (negate (fromInteger contribution))
            | contribution > 0
            ]
    pure
        ( cashFlow start (negate (fromInteger principal))
            : intermediate <> [cashFlow (addDays terminalDay start) terminal]
        )

scaleCashFlows :: Integer -> [CashFlow] -> [CashFlow]
scaleCashFlows scale = fmap (\flow -> cashFlow (cashFlowDate flow) (fromInteger scale * cashFlowAmount flow))

shiftCashFlows :: Integer -> [CashFlow] -> [CashFlow]
shiftCashFlows offset = fmap (\flow -> cashFlow (addDays offset (cashFlowDate flow)) (cashFlowAmount flow))

decimalDouble :: Decimal -> Double
decimalDouble = realToFrac

origin :: Day
origin = ModifiedJulianDay 0
