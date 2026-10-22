module Lib.Time exposing
    ( formatDate
    , groupByKey
    , monthLabel
    )


formatDate : { year : Int, month : Int, day : Int } -> String
formatDate date =
    String.fromInt date.year
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt date.month)
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt date.day)


groupByKey : (a -> String) -> List a -> List ( String, List a )
groupByKey toKey items =
    case items of
        [] ->
            []

        first :: _ ->
            let
                key =
                    String.left 7 (toKey first)

                ( matching, remaining ) =
                    splitByKey key toKey items
            in
            ( key, matching ) :: groupByKey toKey remaining


splitByKey : String -> (a -> String) -> List a -> ( List a, List a )
splitByKey key toKey items =
    case items of
        [] ->
            ( [], [] )

        first :: rest ->
            if String.left 7 (toKey first) == key then
                let
                    ( matching, remaining ) =
                        splitByKey key toKey rest
                in
                ( first :: matching, remaining )

            else
                ( [], items )


monthLabel : String -> String
monthLabel value =
    case String.split "-" value of
        [ year, month ] ->
            monthName month ++ " " ++ year

        _ ->
            value


monthName : String -> String
monthName month =
    case month of
        "01" ->
            "Jan"

        "02" ->
            "Feb"

        "03" ->
            "March"

        "04" ->
            "April"

        "05" ->
            "May"

        "06" ->
            "June"

        "07" ->
            "July"

        "08" ->
            "Aug"

        "09" ->
            "Sept"

        "10" ->
            "Oct"

        "11" ->
            "Nov"

        "12" ->
            "Dec"

        _ ->
            month
