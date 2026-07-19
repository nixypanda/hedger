module Data.Time exposing
    ( Depth(..)
    , FiscalYearStartMonth(..)
    , Month(..)
    , Period(..)
    , depthFromInt
    , depthValue
    , fiscalYearStartMonthFromInt
    , fiscalYearStartMonthValue
    , monthFromDate
    , monthFromString
    , monthToString
    , periodToString
    , shiftMonth
    , shiftPeriod
    )

import Data.Calendar as Calendar
import Data.Date as Date exposing (Date)


type Month
    = Month
        { year : Int
        , month : Int
        }


type Period
    = MonthPeriod Month
    | YearPeriod Int


type Depth
    = Depth Int


type FiscalYearStartMonth
    = FiscalYearStartMonth Int


monthFromDate : Date -> Month
monthFromDate date =
    Date.toString date
        |> String.left 7
        |> monthFromString
        |> Maybe.withDefault (Month { year = 1970, month = 1 })


monthFromString : String -> Maybe Month
monthFromString value =
    case String.split "-" value of
        [ yearText, monthText ] ->
            if
                String.length yearText
                    == 4
                    && String.length monthText
                    == 2
                    && String.all Char.isDigit yearText
                    && String.all Char.isDigit monthText
            then
                Maybe.map2
                    (\year month ->
                        if month >= 1 && month <= 12 then
                            Just (Month { year = year, month = month })

                        else
                            Nothing
                    )
                    (String.toInt yearText)
                    (String.toInt monthText)
                    |> Maybe.andThen identity

            else
                Nothing

        _ ->
            Nothing


monthToString : Month -> String
monthToString (Month value) =
    String.fromInt value.year ++ "-" ++ String.padLeft 2 '0' (String.fromInt value.month)


periodToString : Period -> String
periodToString period =
    case period of
        MonthPeriod month ->
            monthToString month

        YearPeriod year ->
            String.fromInt year


shiftPeriod : Int -> Period -> Period
shiftPeriod offset period =
    case period of
        MonthPeriod month ->
            MonthPeriod (shiftMonth offset month)

        YearPeriod year ->
            YearPeriod (year + offset)


shiftMonth : Int -> Month -> Month
shiftMonth offset month =
    Calendar.shiftMonth offset (monthToString month)
        |> monthFromString
        |> Maybe.withDefault month


depthValue : Depth -> Int
depthValue (Depth value) =
    value


depthFromInt : Int -> Maybe Depth
depthFromInt value =
    if value >= 1 then
        Just (Depth value)

    else
        Nothing


fiscalYearStartMonthFromInt : Int -> Maybe FiscalYearStartMonth
fiscalYearStartMonthFromInt value =
    if value >= 1 && value <= 12 then
        Just (FiscalYearStartMonth value)

    else
        Nothing


fiscalYearStartMonthValue : FiscalYearStartMonth -> Int
fiscalYearStartMonthValue (FiscalYearStartMonth value) =
    value
