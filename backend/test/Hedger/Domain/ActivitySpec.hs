module Hedger.Domain.ActivitySpec (activitySpec) where

import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (addDays, addGregorianMonthsClip, fromGregorian)
import Hedgehog (Gen, Property, assert, failure, footnote, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hedger.Domain.Activity (ActivityRange (..), parseActivityPeriod)
import Hedger.Domain.Period (ReportPeriod (..), monthKey)
import Hedger.Generators (formatYearMonth, genMonth, genYear)

activitySpec :: TestTree
activitySpec =
    testGroup
        "activity"
        [ testGroup
            "parseActivityPeriod MonthActivity"
            [ testProperty "a canonical YYYY-MM is the corresponding calendar month" monthActivityValid
            , testProperty "a malformed YYYY-MM is rejected" monthActivityMalformed
            ]
        , testGroup
            "parseActivityPeriod FiscalYearActivity"
            [ testProperty "the fiscal-start month yields a 12-month period" fiscalYearValid
            , testProperty "a month other than the fiscal start is rejected" fiscalYearMismatch
            , testProperty "a malformed YYYY-MM is rejected" fiscalYearMalformed
            ]
        ]

genFiscalStart :: Gen Int
genFiscalStart = Gen.integral (Range.linear 1 12)

monthActivityValid :: Property
monthActivityValid = property $ do
    year <- forAll genYear
    month <- forAll genMonth
    fiscalStart <- forAll genFiscalStart
    let raw = formatYearMonth year month
        expectedStart = fromGregorian year month 1
    case parseActivityPeriod fiscalStart MonthActivity raw of
        Left err -> footnote (show err) >> failure
        Right period -> do
            period.start === expectedStart
            assert (period.start <= period.end)
            period.key === monthKey period.start
            addDays 1 period.end === addGregorianMonthsClip 1 expectedStart

genMalformedYearMonth :: Gen Text
genMalformedYearMonth =
    Gen.choice [genUnpaddedMonth, genOutOfRangeMonth, genWrongComponentCount]
  where
    genUnpaddedMonth = do
        year <- genYear
        month <- Gen.integral (Range.linear 1 9)
        pure (Text.pack (show year <> "-" <> show (month :: Int)))
    genOutOfRangeMonth = do
        year <- genYear
        month <- Gen.choice [Gen.integral (Range.linear (-50) 0), Gen.integral (Range.linear 13 99)]
        pure (Text.pack (show year <> "-" <> show (month :: Int)))
    genWrongComponentCount = do
        year <- genYear
        month <- genMonth
        let padded = formatYearMonth year month
        Gen.choice
            [ pure (Text.filter (/= '-') padded)
            , pure (padded <> "-01")
            , pure ("-" <> padded)
            , pure ""
            ]

monthActivityMalformed :: Property
monthActivityMalformed = property $ do
    fiscalStart <- forAll genFiscalStart
    raw <- forAll genMalformedYearMonth
    assert (isLeft (parseActivityPeriod fiscalStart MonthActivity raw))

fiscalYearValid :: Property
fiscalYearValid = property $ do
    fiscalStart <- forAll genFiscalStart
    year <- forAll genYear
    let raw = formatYearMonth year fiscalStart
    case parseActivityPeriod fiscalStart FiscalYearActivity raw of
        Left err -> footnote (show err) >> failure
        Right period -> do
            assert (period.start <= period.end)
            addGregorianMonthsClip 12 period.start === addDays 1 period.end

fiscalYearMismatch :: Property
fiscalYearMismatch = property $ do
    fiscalStart <- forAll genFiscalStart
    month <- forAll (Gen.filter (/= fiscalStart) genMonth)
    year <- forAll genYear
    let raw = formatYearMonth year month
    assert (isLeft (parseActivityPeriod fiscalStart FiscalYearActivity raw))

fiscalYearMalformed :: Property
fiscalYearMalformed = property $ do
    fiscalStart <- forAll genFiscalStart
    raw <- forAll genMalformedYearMonth
    assert (isLeft (parseActivityPeriod fiscalStart FiscalYearActivity raw))
