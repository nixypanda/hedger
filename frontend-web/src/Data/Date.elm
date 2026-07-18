module Data.Date exposing (Date(..), decoder, toString)

import Json.Decode as Decode exposing (Decoder)


type Date
    = Date
        { year : Int
        , month : Int
        , day : Int
        }


toString : Date -> String
toString (Date { year, month, day }) =
    String.join "-"
        [ String.fromInt year
        , String.padLeft 2 '0' (String.fromInt month)
        , String.padLeft 2 '0' (String.fromInt day)
        ]


decoder : Decoder Date
decoder =
    Decode.string
        |> Decode.andThen parseDate


parseDate : String -> Decoder Date
parseDate value =
    case String.split "-" value of
        [ yearText, monthText, dayText ] ->
            case ( String.toInt yearText, String.toInt monthText, String.toInt dayText ) of
                ( Just year, Just month, Just day ) ->
                    if
                        String.length yearText
                            == 4
                            && String.length monthText
                            == 2
                            && String.length dayText
                            == 2
                            && validDate year month day
                    then
                        Decode.succeed (Date { year = year, month = month, day = day })

                    else
                        Decode.fail "Expected a valid ISO date."

                _ ->
                    Decode.fail "Expected a valid ISO date."

        _ ->
            Decode.fail "Expected a valid ISO date."


validDate : Int -> Int -> Int -> Bool
validDate year month day =
    month >= 1 && month <= 12 && day >= 1 && day <= daysInMonth year month


daysInMonth : Int -> Int -> Int
daysInMonth year month =
    case month of
        2 ->
            if modBy 400 year == 0 || (modBy 4 year == 0 && modBy 100 year /= 0) then
                29

            else
                28

        4 ->
            30

        6 ->
            30

        9 ->
            30

        11 ->
            30

        _ ->
            31
