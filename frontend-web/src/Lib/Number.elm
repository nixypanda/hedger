module Lib.Number exposing (fixedRoundedString, roundedString)


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
