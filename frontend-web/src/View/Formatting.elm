module View.Formatting exposing
    ( commodityDisplaySymbol
    , compactValue
    , formatAmountDisplay
    , formatBalance
    , roundedString
    )


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


formatBalance : { commodity : String, quantity : String } -> String
formatBalance balance =
    case String.toFloat balance.quantity of
        Just quantity ->
            formatAmountDisplay
                (commodityDisplaySymbol balance.commodity)
                quantity

        Nothing ->
            commodityDisplaySymbol balance.commodity ++ " " ++ balance.quantity


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
    in
    sign ++ symbol ++ value ++ suffix


compactValue : String -> Float -> ( String, String )
compactValue commodity amount =
    if commodity == "₹" && amount >= 10000000 then
        ( roundedString (amount / 10000000) 1, "Cr" )

    else if commodity == "₹" && amount >= 100000 then
        ( roundedString (amount / 100000) 1, "L" )

    else if amount >= 1000000000 then
        ( roundedString (amount / 1000000000) 1, "B" )

    else if amount >= 1000000 then
        ( roundedString (amount / 1000000) 1, "M" )

    else if amount >= 1000 then
        ( roundedString (amount / 1000) 1, "K" )

    else
        ( roundedString amount 2, "" )


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
