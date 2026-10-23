module FormattingTest exposing (suite)

import Data.Decimal as Decimal
import Data.Formatting as Formatting
import Expect
import Fuzz
import Lib.Number as Number
import Test exposing (Test, describe, fuzz, test)


{-| Money is displayed at a fixed domain precision (two decimals for full
amounts) and a fixed domain scale (K/M/B, with ₹ lakh/crore). These are the
rules the ledger relies on for readable, unambiguous amounts.
-}
suite : Test
suite =
    describe "amount formatting"
        [ fuzz displayFuzzer "amounts use the domain precision and scale" displayFormattingHolds
        , test "annual rates render as percentages" <|
            \_ ->
                Decimal.fromString "0.07"
                    |> Result.map Formatting.formatRatePercentage
                    |> Expect.equal (Ok "7%")
        , test "generic rounding trims trailing zeros" <|
            \_ -> roundedStringHolds
        , test "axis values omit the symbol and keep the sign" <|
            \_ -> axisValuesHold
        , test "decimal axis values include the symbol" <|
            \_ -> decimalAxisHolds
        , test "missing decimals render as an em dash" <|
            \_ -> missingDecimalHolds
        , test "present decimals keep the symbol and precision" <|
            \_ -> presentDecimalHolds
        , test "period helpers preserve month and year forms" <|
            \_ -> periodHelpersHold
        ]


type alias DisplayInput =
    { decimal : DecimalInput
    , compact : CompactCase
    }


type alias DecimalInput =
    { negative : Bool
    , whole : Int
    , hundredths : Int
    }


type alias CompactCase =
    { symbol : String
    , exp : Int
    , mantissa : Int
    , suffix : String
    }


displayFuzzer : Fuzz.Fuzzer DisplayInput
displayFuzzer =
    Fuzz.map2 DisplayInput decimalFuzzer compactFuzzer


decimalFuzzer : Fuzz.Fuzzer DecimalInput
decimalFuzzer =
    Fuzz.map3 DecimalInput
        Fuzz.bool
        (Fuzz.intRange 0 1000000000)
        (Fuzz.intRange 0 99)


compactFuzzer : Fuzz.Fuzzer CompactCase
compactFuzzer =
    Fuzz.oneOf
        [ compactCase "$" 0 0 999 ""
        , compactCase "$" 3 1 999 "K"
        , compactCase "$" 6 1 999 "M"
        , compactCase "$" 9 1 999 "B"
        , compactCase "₹" 5 1 99 "L"
        , compactCase "₹" 7 1 999 "Cr"
        ]


compactCase : String -> Int -> Int -> Int -> String -> Fuzz.Fuzzer CompactCase
compactCase symbol exp low high suffix =
    Fuzz.map
        (\mantissa ->
            { symbol = symbol, exp = exp, mantissa = mantissa, suffix = suffix }
        )
        (Fuzz.intRange low high)


displayFormattingHolds : DisplayInput -> Expect.Expectation
displayFormattingHolds input =
    Expect.all
        [ \_ -> fullAmountHolds input.decimal
        , \_ -> compactAmountHolds input.compact
        ]
        ()


fullAmountHolds : DecimalInput -> Expect.Expectation
fullAmountHolds input =
    let
        raw =
            (if input.negative then
                "-"

             else
                ""
            )
                ++ String.fromInt input.whole
                ++ "."
                ++ String.padLeft 2 '0' (String.fromInt input.hundredths)

        expected =
            (if input.negative then
                -1

             else
                1
            )
                * (toFloat input.whole + toFloat input.hundredths / 100)
    in
    case Decimal.fromString raw of
        Err error ->
            Expect.fail ("Fixture was not a decimal: " ++ error)

        Ok amount ->
            let
                result =
                    Formatting.formatFullValue amount
            in
            Expect.all
                [ \value -> Expect.equal 2 (fractionDigitCount value)
                , expectNoExponent
                , expectRoundedTo expected
                ]
                result


compactAmountHolds : CompactCase -> Expect.Expectation
compactAmountHolds input =
    let
        scale =
            toFloat (10 ^ input.exp)

        amount =
            toFloat input.mantissa * scale

        ( value, suffix ) =
            Formatting.compactValue input.symbol amount

        rendered =
            String.toFloat value |> Maybe.withDefault -1

        expectedValue =
            toFloat input.mantissa
    in
    Expect.equal
        { suffix = suffix, valueMatches = abs (rendered - expectedValue) <= 0.05 }
        { suffix = input.suffix, valueMatches = True }


fractionDigitCount : String -> Int
fractionDigitCount value =
    case String.split "." value of
        [ _, fraction ] ->
            String.length fraction

        _ ->
            0


expectNoExponent : String -> Expect.Expectation
expectNoExponent value =
    Expect.equal False (String.contains "e" value || String.contains "E" value)


expectRoundedTo : Float -> String -> Expect.Expectation
expectRoundedTo expected value =
    Expect.equal True (abs (renderedFloat value - expected) <= 0.005)


renderedFloat : String -> Float
renderedFloat value =
    String.toFloat value |> Maybe.withDefault 0


roundedStringHolds : Expect.Expectation
roundedStringHolds =
    [ Number.roundedString 1.5 0
    , Number.roundedString 1.25 1
    , Number.roundedString 100 2
    ]
        |> Expect.equal [ "2", "1.3", "100" ]


axisValuesHold : Expect.Expectation
axisValuesHold =
    [ Formatting.formatAxisValue "INR" 100000
    , Formatting.formatAxisValue "USD" -1500
    ]
        |> Expect.equal [ "1.0L", "-1.5K" ]


decimalAxisHolds : Expect.Expectation
decimalAxisHolds =
    Formatting.formatDecimalAxis "INR" 100000
        |> Expect.equal "₹1.0L"


missingDecimalHolds : Expect.Expectation
missingDecimalHolds =
    Formatting.formatMaybeDecimalDisplay "USD" Nothing
        |> Expect.equal "—"


presentDecimalHolds : Expect.Expectation
presentDecimalHolds =
    Formatting.formatMaybeDecimalDisplay "INR" (Just Decimal.zero)
        |> Expect.equal "₹0.00"


periodHelpersHold : Expect.Expectation
periodHelpersHold =
    [ Formatting.shortPeriod "2026-03"
    , Formatting.shortPeriodWithYear "2026-03"
    , Formatting.periodDisplayName "2026-03"
    ]
        |> Expect.equal [ "03", "03/26", "March 2026" ]
