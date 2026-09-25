module View.Formatting exposing
    ( commodityDisplaySymbol
    , compactValue
    , formatAmountDisplay
    , formatBalance
    , formatFullAmount
    , formatFullBalance
    , formatFullValue
    , formatPercentage
    , formatRatePercentage
    , roundedString
    )

import Char
import Data.Decimal as Decimal exposing (Decimal)
import Data.Quantity as Quantity exposing (Quantity)


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
        |> Maybe.map (\value -> roundedString (value * multiplier) 2 ++ "%")
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
        ( fixedRoundedString (amount / 10000000) 1, "Cr" )

    else if commodity == "₹" && amount >= 100000 then
        ( fixedRoundedString (amount / 100000) 1, "L" )

    else if amount >= 1000000000 then
        ( fixedRoundedString (amount / 1000000000) 1, "B" )

    else if amount >= 1000000 then
        ( fixedRoundedString (amount / 1000000) 1, "M" )

    else if amount >= 1000 then
        ( fixedRoundedString (amount / 1000) 1, "K" )

    else
        ( fixedRoundedString amount 2, "" )


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


fixedRoundedString : Float -> Int -> String
fixedRoundedString value decimals =
    let
        rounded =
            roundedString value decimals
    in
    case String.split "." rounded of
        [ whole, fraction ] ->
            whole ++ "." ++ String.padRight decimals '0' fraction

        [ whole ] ->
            if decimals == 0 then
                whole

            else
                whole ++ "." ++ String.repeat decimals "0"

        _ ->
            rounded


roundedString : Float -> Int -> String
roundedString value decimals =
    let
        factor =
            10 ^ decimals

        rounded =
            toFloat (round (value * toFloat factor)) / toFloat factor

        raw =
            String.fromFloat rounded
    in
    if String.endsWith ".0" raw then
        String.dropRight 2 raw

    else
        raw
