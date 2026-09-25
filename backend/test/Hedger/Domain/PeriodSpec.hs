module Hedger.Domain.PeriodSpec (
    periodSpec,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, addDays, fromGregorian, toGregorian)
import Hedgehog (Gen, Property, assert, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hedger.Domain.Period (
    ReportPeriod (..),
    atIndex,
    lastMaybe,
    maximumMaybe,
    minimumMaybe,
    monthKey,
    monthPeriod,
    monthStart,
    parseYearMonth,
    takeLast,
 )
import Hedger.Generators (formatYearMonth, genDay, genMonth, genYear)

periodSpec :: TestTree
periodSpec =
    testGroup
        "period"
        [ testGroup
            "parseYearMonth / monthKey"
            [ testProperty
                "parseYearMonth . monthKey round-trips through toGregorian"
                monthKeyRoundTrip
            , testProperty "malformed inputs are always rejected" rejectsMalformed
            ]
        , testGroup
            "monthStart"
            [ testProperty "never later than the input day" monthStartLE
            , testProperty "keeps the same (year, month) and lands on day 1" monthStartSameMonth
            ]
        , testGroup
            "monthPeriod"
            [ testProperty
                "start, end and key are consistent with the input day"
                monthPeriodConsistent
            ]
        , testGroup
            "takeLast"
            [ testProperty "matches reverse . take (max 0 n) . reverse" takeLastMatchesReverseTake
            ]
        , testGroup
            "atIndex"
            [ testProperty "in-range index matches (!!)" atIndexInRange
            , testProperty "out-of-range index is Nothing" atIndexOutOfRange
            ]
        , testGroup
            "maximumMaybe / minimumMaybe / lastMaybe"
            [ testProperty "Nothing iff the list is empty" extremeNothingIffEmpty
            , testProperty "maximumMaybe is a member and an upper bound" maximumMaybeIsBound
            , testProperty "minimumMaybe is a member and a lower bound" minimumMaybeIsBound
            ]
        ]

-- Day generation
monthKeyRoundTrip :: Property
monthKeyRoundTrip =
    property $ do
        day <- forAll genDay
        let (year, month, _) = toGregorian day
        parseYearMonth (monthKey day) === Just (year, month)

genUnpaddedMonth :: Gen Text
genUnpaddedMonth = do
    year <- genYear
    month <- Gen.integral (Range.linear 1 9)
    pure (Text.pack (show year <> "-" <> show month))

genOutOfRangeMonth :: Gen Text
genOutOfRangeMonth = do
    year <- genYear
    month <- Gen.choice [Gen.integral (Range.linear (-50) 0), Gen.integral (Range.linear 13 99)]
    pure (Text.pack (show year <> "-" <> show (month :: Int)))

genWrongComponentCount :: Gen Text
genWrongComponentCount = do
    year <- genYear
    month <- genMonth
    let padded = formatYearMonth year month
    Gen.choice
        [pure (Text.filter (/= '-') padded), pure (padded <> "-01"), pure ("-" <> padded), pure ""]

genAlphaToken :: Gen Text
genAlphaToken = Text.pack <$> Gen.list (Range.linear 1 5) Gen.alpha

genNumericToken :: Gen Text
genNumericToken = Text.pack . show <$> Gen.integral (Range.linear 0 9999 :: Range.Range Integer)

genNonNumericParts :: Gen Text
genNonNumericParts = do
    alphaIsYear <- Gen.bool
    yearPart <-
        if alphaIsYear
            then genAlphaToken
            else genNumericToken
    monthPart <-
        if alphaIsYear
            then Gen.choice [genNumericToken, genAlphaToken]
            else genAlphaToken
    pure (yearPart <> "-" <> monthPart)

genMalformedYearMonth :: Gen Text
genMalformedYearMonth =
    Gen.choice [genUnpaddedMonth, genOutOfRangeMonth, genWrongComponentCount, genNonNumericParts]

rejectsMalformed :: Property
rejectsMalformed =
    property $ do
        raw <- forAll genMalformedYearMonth
        parseYearMonth raw === Nothing

monthStartLE :: Property
monthStartLE =
    property $ do
        day <- forAll genDay
        assert (monthStart day <= day)

monthStartSameMonth :: Property
monthStartSameMonth =
    property $ do
        day <- forAll genDay
        let (year, month, _) = toGregorian day
            (startYear, startMonth, startDom) = toGregorian (monthStart day)
        (year, month) === (startYear, startMonth)
        startDom === 1

-- | The first day of the calendar month immediately following the given day.
nextMonthFirstDay :: Day -> Day
nextMonthFirstDay day =
    let (year, month, _) = toGregorian day
     in if month == 12
            then fromGregorian (year + 1) 1 1
            else fromGregorian year (month + 1) 1

monthPeriodConsistent :: Property
monthPeriodConsistent =
    property $ do
        day <- forAll genDay
        let period = monthPeriod day
        period.start === monthStart day
        assert (period.start <= period.end)
        period.key === monthKey period.start
        assert (period.start <= day && day <= period.end)
        addDays 1 period.end === nextMonthFirstDay period.start

-- takeLast
genIntList :: Gen [Int]
genIntList = Gen.list (Range.linear 0 50) (Gen.integral (Range.linearFrom 0 (-1000) 1000))

genTakeCount :: Gen Int
genTakeCount = Gen.integral (Range.linearFrom 0 (-10) 60)

takeLastMatchesReverseTake :: Property
takeLastMatchesReverseTake =
    property $ do
        xs <- forAll genIntList
        n <- forAll genTakeCount
        takeLast n xs === reverse (take (max 0 n) (reverse xs))

-- atIndex
atIndexInRange :: Property
atIndexInRange =
    property $ do
        xs <- forAll (Gen.filter (not . null) genIntList)
        index <- forAll (Gen.integral (Range.linear 0 (length xs - 1)))
        atIndex index xs === Just (xs !! index)

atIndexOutOfRange :: Property
atIndexOutOfRange =
    property $ do
        xs <- forAll genIntList
        offset <- forAll (Gen.integral (Range.linear 0 100))
        index <- forAll (Gen.choice [pure (negate (offset + 1)), pure (length xs + offset)])
        atIndex index xs === Nothing

-- maximumMaybe / minimumMaybe / lastMaybe
extremeNothingIffEmpty :: Property
extremeNothingIffEmpty =
    property $ do
        xs <- forAll genIntList
        let isEmpty = null xs
        (maximumMaybe xs == Nothing) === isEmpty
        (minimumMaybe xs == Nothing) === isEmpty
        (lastMaybe xs == Nothing) === isEmpty

maximumMaybeIsBound :: Property
maximumMaybeIsBound =
    property $ do
        xs <- forAll (Gen.filter (not . null) genIntList)
        case maximumMaybe xs of
            Nothing -> assert False
            Just result -> do
                assert (result `elem` xs)
                assert (all (<= result) xs)

minimumMaybeIsBound :: Property
minimumMaybeIsBound =
    property $ do
        xs <- forAll (Gen.filter (not . null) genIntList)
        case minimumMaybe xs of
            Nothing -> assert False
            Just result -> do
                assert (result `elem` xs)
                assert (all (>= result) xs)
