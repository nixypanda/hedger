module Data.Calendar exposing
    ( daysInMonth
    , fiscalPeriodForMonth
    , fiscalPeriodLabel
    , monthCells
    , monthWeeks
    , periodLabel
    , shiftMonth
    , shiftPeriod
    )

import Data.Granularity exposing (Granularity(..))
import Lib.ListExtra as ListExtra
import Lib.Time as Time


shiftPeriod : Granularity -> Int -> String -> String
shiftPeriod granularity offset period =
    case granularity of
        Monthly ->
            shiftMonth offset period

        Yearly ->
            String.toInt period
                |> Maybe.map ((+) offset >> String.fromInt)
                |> Maybe.withDefault period


shiftMonth : Int -> String -> String
shiftMonth offset period =
    case parseMonth period of
        Just ( year, month ) ->
            let
                zeroBased =
                    year * 12 + month - 1 + offset

                nextYear =
                    floor (toFloat zeroBased / 12)

                nextMonth =
                    modBy 12 zeroBased + 1
            in
            monthKey nextYear nextMonth

        Nothing ->
            period


fiscalPeriodForMonth : Int -> String -> String
fiscalPeriodForMonth fiscalStart period =
    case parseMonth period of
        Just ( year, month ) ->
            monthKey
                (if month >= fiscalStart then
                    year

                 else
                    year - 1
                )
                fiscalStart

        Nothing ->
            period


fiscalPeriodLabel : String -> String
fiscalPeriodLabel period =
    case parseMonth period of
        Just ( year, _ ) ->
            "FY "
                ++ String.fromInt year
                ++ "–"
                ++ String.right 2 (String.fromInt (year + 1))

        Nothing ->
            period


periodLabel : Granularity -> String -> String
periodLabel granularity period =
    case granularity of
        Monthly ->
            Time.monthLabel period

        Yearly ->
            period


{-| The real cells for a month, padded only enough to complete the final week.
The calendar view decides how many week rows to render and how to share a fixed
height between them.
-}
monthCells : String -> List (Maybe Int)
monthCells period =
    case parseMonth period of
        Just ( year, month ) ->
            let
                cells =
                    List.repeat (dayOfWeek year month 1) Nothing
                        ++ List.map Just (List.range 1 (daysInMonth year month))

                trailing =
                    modBy 7 (7 - modBy 7 (List.length cells))
            in
            cells ++ List.repeat trailing Nothing

        Nothing ->
            []


{-| The actual week rows for a month, without a blank trailing week.
-}
monthWeeks : String -> List (List (Maybe Int))
monthWeeks period =
    monthCells period |> ListExtra.chunksOf 7


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


dayOfWeek : Int -> Int -> Int -> Int
dayOfWeek year month day =
    let
        adjustedYear =
            if month < 3 then
                year - 1

            else
                year

        adjustedMonth =
            if month < 3 then
                month + 12

            else
                month

        zeroSunday =
            day
                + floor (toFloat (13 * (adjustedMonth + 1)) / 5)
                + adjustedYear
                + floor (toFloat adjustedYear / 4)
                - floor (toFloat adjustedYear / 100)
                + floor (toFloat adjustedYear / 400)
    in
    modBy 7 (zeroSunday + 6)


parseMonth : String -> Maybe ( Int, Int )
parseMonth value =
    case String.split "-" value of
        [ yearText, monthText ] ->
            Maybe.map2 Tuple.pair (String.toInt yearText) (String.toInt monthText)

        _ ->
            Nothing


monthKey : Int -> Int -> String
monthKey year month =
    String.fromInt year ++ "-" ++ String.padLeft 2 '0' (String.fromInt month)
