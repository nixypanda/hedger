module FormattingTest exposing (suite)

import Data.Decimal as Decimal
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)
import View.Formatting as Formatting


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
