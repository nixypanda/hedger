module Hedger.Domain.ValuationSpec (
    valuationSpec,
) where

import Data.Decimal (Decimal)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Hedgehog (Gen, Property, assert, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Hedgehog (testProperty)

import Hledger.Data.Amount (amountsRaw, mixed, num)
import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Valuation (
    Conversion (..),
    completeDecimal,
    conversionFromMixed,
    conversionMissing,
    conversionValue,
    foldMapConversion,
    missingCommodities,
 )
import Hedger.Generators (displayCommodity, genCommodity, genMixedAmount, genQuantity)

valuationSpec :: TestTree
valuationSpec =
    testGroup
        "valuation"
        [ testGroup
            "conversionFromMixed"
            [ testProperty "missing is the set of foreign commodities" conversionFromMixedMissing
            , testProperty
                "value is the sum of quantities when nothing is missing"
                conversionFromMixedSumWhenComplete
            , testCase
                "a sub-epsilon foreign residual is not flagged missing"
                conversionFromMixedIgnoresDustResidual
            ]
        , testGroup
            "foldMapConversion"
            [ testProperty
                "missing is the union of the parts' missing sets"
                foldMapConversionMissing
            , testProperty "value collects the Just values in order" foldMapConversionValue
            ]
        , testGroup
            "completeDecimal / missingCommodities"
            [testProperty "Just iff missingCommodities is empty" completeDecimalConsistency]
        ]

-- Helpers
genMixedAmountInDisplayCommodity :: Gen Hledger.MixedAmount
genMixedAmountInDisplayCommodity = mixed <$> Gen.list (Range.linear 0 6) genAmountInDisplayCommodity
  where
    genAmountInDisplayCommodity = do
        quantity <- genQuantity
        pure (num quantity){Hledger.acommodity = displayCommodity}

{- | Generators for a non-empty commodity set and each 'Conversion' constructor,
so every state and both the complete and incomplete branches are exercised.
-}
genNonEmptyCommodities :: Gen (NonEmpty Text)
genNonEmptyCommodities = do
    first <- genCommodity
    rest <- Gen.list (Range.linear 0 3) genCommodity
    pure (first :| rest)

genConversion :: Gen (Conversion Decimal)
genConversion =
    Gen.choice
        [ Converted <$> genQuantity
        , PartlyConverted <$> genQuantity <*> genNonEmptyCommodities
        , Unconverted <$> genNonEmptyCommodities
        , pure NotApplicable
        ]

-- conversionFromMixed
conversionFromMixedMissing :: Property
conversionFromMixedMissing =
    property $ do
        amount <- forAll genMixedAmount
        let missing = conversionMissing (conversionFromMixed displayCommodity amount)
            commodities = [item.acommodity | item <- amountsRaw amount]
        -- the display commodity is never reported missing
        assert (displayCommodity `Set.notMember` missing)
        -- everything reported missing is a foreign commodity that is actually present
        assert
            ( all
                ( \commodity ->
                    commodity /= displayCommodity && commodity `elem` commodities
                )
                missing
            )
        -- conversionFromMixed and missingCommodities agree on which commodities are missing,
        -- so a non-negligible foreign commodity is missing and a dust residual is not
        missing === missingCommodities displayCommodity amount

conversionFromMixedSumWhenComplete :: Property
conversionFromMixedSumWhenComplete =
    property $ do
        amount <- forAll genMixedAmountInDisplayCommodity
        let conversion = conversionFromMixed displayCommodity amount
            expected = sum [item.aquantity | item <- amountsRaw amount]
        conversionValue conversion === Just (expected :: Decimal)

{- | A foreign-commodity residual below the 1e-6 magnitude epsilon that
'completeDecimal' and 'missingCommodities' ignore should not appear in
'conversionFromMixed''s missing set either; today it has no epsilon at all,
so this dust component is spuriously flagged missing.
-}
conversionFromMixedIgnoresDustResidual :: IO ()
conversionFromMixedIgnoresDustResidual =
    let amount =
            mixed
                [ (num 500000){Hledger.acommodity = displayCommodity}
                , (num 0.0000004){Hledger.acommodity = "EUR"}
                ]
        conversion = conversionFromMixed displayCommodity amount
     in assertEqual
            "a sub-1e-6 foreign residual should not be reported as missing"
            Set.empty
            (conversionMissing conversion)

-- foldMapConversion
genConversions :: Gen [Conversion Decimal]
genConversions = Gen.list (Range.linear 0 5) genConversion

foldMapConversionMissing :: Property
foldMapConversionMissing =
    property $ do
        conversions <- forAll genConversions
        let result = foldMapConversion conversions
        conversionMissing result === Set.unions (fmap conversionMissing conversions)

foldMapConversionValue :: Property
foldMapConversionValue =
    property $ do
        conversions <- forAll genConversions
        let result = foldMapConversion conversions
        conversionValue result === Just (mapMaybe conversionValue conversions)

-- completeDecimal / missingCommodities
completeDecimalConsistency :: Property
completeDecimalConsistency =
    property $ do
        amount <- forAll genMixedAmount
        isJust (completeDecimal displayCommodity amount)
            === Set.null (missingCommodities displayCommodity amount)
