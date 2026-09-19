module Hedger.Domain.ValuationSpec (
    valuationSpec,
) where

import Data.Decimal (Decimal)
import Data.Maybe (isJust, mapMaybe)
import Data.Set qualified as Set
import Hedgehog (Gen, Property, assert, failure, forAll, property, (===))
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
    foldMapConversion,
    missingCommodities,
    requireComplete,
 )
import Hedger.Generators (displayCommodity, genCommodity, genMixedAmount, genQuantity)

valuationSpec :: TestTree
valuationSpec =
    testGroup
        "valuation"
        [ testGroup
            "Conversion Functor"
            [ testProperty "fmap id leaves value and missing unchanged" functorIdentity
            , testProperty "fmap (f . g) equals fmap f . fmap g" functorComposition
            ]
        , testGroup
            "requireComplete"
            [ testProperty "idempotent" requireCompleteIdempotent
            , testProperty "preserves missing" requireCompletePreservesMissing
            , testProperty "nulls the value iff missing is non-empty" requireCompleteNullsValue
            ]
        , testGroup
            "conversionFromMixed"
            [ testProperty "value is always Just" conversionFromMixedValueIsJust
            , testProperty "missing is the set of foreign commodities" conversionFromMixedMissing
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
            , testProperty "value is always Just" foldMapConversionValueIsJust
            , testProperty "collected values drop the Nothing parts" foldMapConversionDropsNothing
            , testProperty
                "each part's missing is a subset of the result's missing"
                foldMapConversionMissingSuperset
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

{- | A 'Conversion' built directly from its fields, so both the 'Nothing' value branch and
arbitrary (possibly empty, possibly overlapping) missing sets are exercised.
-}
genConversion :: Gen (Conversion Decimal)
genConversion = do
    value <- Gen.maybe genQuantity
    missing <- Set.fromList <$> Gen.list (Range.linear 0 4) genCommodity
    pure Conversion{value, missing}

-- Conversion Functor laws
functorIdentity :: Property
functorIdentity =
    property $ do
        conversion <- forAll genConversion
        let applied = fmap id conversion
        applied.value === conversion.value
        applied.missing === conversion.missing

functorComposition :: Property
functorComposition =
    property $ do
        conversion <- forAll genConversion
        let f = (+ 1)
            g = (* 2)
            composed = fmap (f . g) conversion
            sequenced = fmap f (fmap g conversion)
        composed.value === sequenced.value
        composed.missing === sequenced.missing

-- requireComplete
requireCompleteIdempotent :: Property
requireCompleteIdempotent =
    property $ do
        conversion <- forAll genConversion
        let once = requireComplete conversion
            twice = requireComplete once
        once.value === twice.value
        once.missing === twice.missing

requireCompletePreservesMissing :: Property
requireCompletePreservesMissing =
    property $ do
        conversion <- forAll genConversion
        (requireComplete conversion).missing === conversion.missing

requireCompleteNullsValue :: Property
requireCompleteNullsValue =
    property $ do
        conversion <- forAll genConversion
        let result = requireComplete conversion
        if Set.null conversion.missing
            then result.value === conversion.value
            else result.value === Nothing

-- conversionFromMixed
conversionFromMixedValueIsJust :: Property
conversionFromMixedValueIsJust =
    property $ do
        amount <- forAll genMixedAmount
        let conversion = conversionFromMixed displayCommodity amount
        isJust conversion.value === True

conversionFromMixedMissing :: Property
conversionFromMixedMissing =
    property $ do
        amount <- forAll genMixedAmount
        let missing = (conversionFromMixed displayCommodity amount).missing
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
        conversion.value === Just (expected :: Decimal)

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
            conversion.missing

-- foldMapConversion
genConversions :: Gen [Conversion Decimal]
genConversions = Gen.list (Range.linear 0 5) genConversion

foldMapConversionMissing :: Property
foldMapConversionMissing =
    property $ do
        conversions <- forAll genConversions
        let result = foldMapConversion conversions
        result.missing === Set.unions (fmap (.missing) conversions)

foldMapConversionValue :: Property
foldMapConversionValue =
    property $ do
        conversions <- forAll genConversions
        let result = foldMapConversion conversions
        result.value === Just (mapMaybe (.value) conversions)

foldMapConversionValueIsJust :: Property
foldMapConversionValueIsJust =
    property $ do
        conversions <- forAll genConversions
        assert (isJust (foldMapConversion conversions).value)

foldMapConversionDropsNothing :: Property
foldMapConversionDropsNothing =
    property $ do
        conversions <- forAll genConversions
        let justCount = length (mapMaybe (.value) conversions)
        case (foldMapConversion conversions).value of
            Nothing -> failure
            Just collected -> length collected === justCount

foldMapConversionMissingSuperset :: Property
foldMapConversionMissingSuperset =
    property $ do
        conversions <- forAll genConversions
        let result = foldMapConversion conversions
        assert (all (\part -> part.missing `Set.isSubsetOf` result.missing) conversions)

-- completeDecimal / missingCommodities
completeDecimalConsistency :: Property
completeDecimalConsistency =
    property $ do
        amount <- forAll genMixedAmount
        isJust (completeDecimal displayCommodity amount)
            === Set.null (missingCommodities displayCommodity amount)
