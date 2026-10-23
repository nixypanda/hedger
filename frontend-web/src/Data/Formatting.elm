module Data.Formatting exposing
    ( compactValue
    , formatAxisValue
    , formatBalance
    , formatDecimalAxis
    , formatDecimalDisplay
    , formatFloatDisplay
    , formatFullAmount
    , formatFullBalance
    , formatFullValue
    , formatMaybeDecimalDisplay
    , formatPercentage
    , formatRatePercentage
    , periodDisplayName
    , shortPeriod
    , shortPeriodWithYear
    )

import Char
import Data.Calendar as Calendar
import Data.Decimal as Decimal exposing (Decimal)
import Data.Granularity exposing (Granularity(..))
import Data.Quantity as Quantity exposing (Quantity)
import Lib.Number as Number


commodityDisplaySymbol : String -> String
commodityDisplaySymbol commodity =
    case commodity of
        "USD" ->
            "$"

        "INR" ->
            "₹"

        "EUR" ->
            "€"

        "GBP" ->
            "£"

        "JPY" ->
            "¥"

        "CNY" ->
            "¥"

        "BTC" ->
            "₿"

        "CAD" ->
            "CA$"

        "AUD" ->
            "AU$"

        "CHF" ->
            "Fr"

        _ ->
            commodity


formatBalance : Quantity -> String
formatBalance balance =
    let
        quantityStr =
            Decimal.toString (Quantity.quantity balance)

        quantityFloat =
            String.toFloat quantityStr
    in
    case quantityFloat of
        Just quantity ->
            formatAmountDisplay
                (commodityDisplaySymbol (Quantity.commodity balance))
                quantity

        Nothing ->
            commodityDisplaySymbol (Quantity.commodity balance) ++ " " ++ quantityStr


formatPercentage : Decimal -> String
formatPercentage percentage =
    formatPercentageWithMultiplier 1 percentage


formatRatePercentage : Decimal -> String
formatRatePercentage rate =
    formatPercentageWithMultiplier 100 rate


formatPercentageWithMultiplier : Float -> Decimal -> String
formatPercentageWithMultiplier multiplier percentage =
    percentage
        |> Decimal.toString
        |> String.toFloat
        |> Maybe.map (\value -> Number.roundedString (value * multiplier) 2 ++ "%")
        |> Maybe.withDefault "—"


formatAmountDisplay : String -> Float -> String
formatAmountDisplay symbol amount =
    let
        absoluteAmount =
            abs amount

        sign =
            if amount < 0 then
                "-"

            else
                ""

        ( value, suffix ) =
            compactValue symbol absoluteAmount

        separator =
            symbolSeparator symbol
    in
    sign ++ symbol ++ separator ++ value ++ suffix


symbolSeparator : String -> String
symbolSeparator symbol =
    case String.right 1 symbol |> String.uncons of
        Just ( c, "" ) ->
            if Char.isAlpha c then
                " "

            else
                ""

        _ ->
            ""


compactValue : String -> Float -> ( String, String )
compactValue commodity amount =
    if commodity == "₹" && amount >= 10000000 then
        ( Number.fixedRoundedString (amount / 10000000) 1, "Cr" )

    else if commodity == "₹" && amount >= 100000 then
        ( Number.fixedRoundedString (amount / 100000) 1, "L" )

    else if amount >= 1000000000 then
        ( Number.fixedRoundedString (amount / 1000000000) 1, "B" )

    else if amount >= 1000000 then
        ( Number.fixedRoundedString (amount / 1000000) 1, "M" )

    else if amount >= 1000 then
        ( Number.fixedRoundedString (amount / 1000) 1, "K" )

    else
        ( Number.fixedRoundedString amount 2, "" )


formatFullAmount : String -> Decimal -> String
formatFullAmount commodity amount =
    formatFullValue amount ++ " " ++ commodity


formatFullBalance : Quantity -> String
formatFullBalance balance =
    let
        symbol =
            commodityDisplaySymbol (Quantity.commodity balance)

        value =
            formatFullValue (Quantity.quantity balance)
    in
    case String.uncons value of
        Just ( '-', digits ) ->
            "-" ++ symbol ++ symbolSeparator symbol ++ digits

        _ ->
            symbol ++ symbolSeparator symbol ++ value


formatFullValue : Decimal -> String
formatFullValue amount =
    let
        decimalPlaces =
            2

        raw =
            Decimal.toString amount

        ( sign, unsigned ) =
            if String.startsWith "-" raw then
                ( "-", String.dropLeft 1 raw )

            else
                ( "", raw )

        ( whole, fraction ) =
            case String.split "." unsigned of
                [ integer, decimal ] ->
                    ( integer, decimal )

                _ ->
                    ( unsigned, "" )

        paddedFraction =
            String.padRight (decimalPlaces + 1) '0' fraction

        keptDigits =
            whole ++ String.left decimalPlaces paddedFraction

        roundedDigits =
            if String.slice decimalPlaces (decimalPlaces + 1) paddedFraction >= "5" then
                incrementDigits keptDigits

            else
                keptDigits

        wholeLength =
            String.length roundedDigits - decimalPlaces
    in
    sign
        ++ String.left wholeLength roundedDigits
        ++ "."
        ++ String.dropLeft wholeLength roundedDigits


incrementDigits : String -> String
incrementDigits digits =
    let
        increment digit ( result, carry ) =
            if not carry then
                ( digit :: result, False )

            else if digit == '9' then
                ( '0' :: result, True )

            else
                ( Char.fromCode (Char.toCode digit + 1) :: result, False )

        ( incremented, overflow ) =
            String.toList digits |> List.foldr increment ( [], True )
    in
    String.fromList
        (if overflow then
            '1' :: incremented

         else
            incremented
        )


formatDecimalDisplay : String -> Decimal -> String
formatDecimalDisplay commodity amount =
    formatAmountDisplay
        (commodityDisplaySymbol commodity)
        (Decimal.toFloat amount)


formatMaybeDecimalDisplay : String -> Maybe Decimal -> String
formatMaybeDecimalDisplay commodity value =
    value
        |> Maybe.map
            (Decimal.toFloat >> formatAmountDisplay (commodityDisplaySymbol commodity))
        |> Maybe.withDefault "—"


formatFloatDisplay : String -> Float -> String
formatFloatDisplay commodity amount =
    formatAmountDisplay (commodityDisplaySymbol commodity) amount


formatAxisValue : String -> Float -> String
formatAxisValue commodity amount =
    let
        ( value, suffix ) =
            compactValue (commodityDisplaySymbol commodity) (abs amount)

        sign =
            if amount < 0 then
                "-"

            else
                ""
    in
    sign ++ value ++ suffix


formatDecimalAxis : String -> Float -> String
formatDecimalAxis commodity =
    formatAmountDisplay (commodityDisplaySymbol commodity)


periodDisplayName : String -> String
periodDisplayName period =
    if String.length period == 7 then
        Calendar.periodLabel Monthly period

    else
        period


shortPeriod : String -> String
shortPeriod period =
    if String.length period == 7 then
        String.right 2 period

    else
        period


shortPeriodWithYear : String -> String
shortPeriodWithYear period =
    case String.split "-" period of
        [ year, month ] ->
            month ++ "/" ++ String.right 2 year

        _ ->
            period
