module Data.ActivityCalendar exposing (yearBuckets)

import Data.Analytics exposing (ActivityBucket)
import Data.Calendar as Calendar
import Data.Date exposing (Date(..))
import Data.Decimal as Decimal
import Data.Time as Time exposing (Month(..))


yearBuckets : String -> List ActivityBucket -> List ActivityBucket
yearBuckets period buckets =
    case latestBucket buckets of
        Nothing ->
            []

        Just finalBucket ->
            monthKeysThrough period finalBucket.key
                |> List.map (bucketFor buckets)


latestBucket : List ActivityBucket -> Maybe ActivityBucket
latestBucket buckets =
    buckets
        |> List.sortBy .key
        |> List.reverse
        |> List.head


monthKeysThrough : String -> String -> List String
monthKeysThrough first final =
    case ( Time.monthFromString first, Time.monthFromString final ) of
        ( Just _, Just _ ) ->
            validMonthKeysThrough first final

        _ ->
            []


validMonthKeysThrough : String -> String -> List String
validMonthKeysThrough current final =
    if current > final then
        []

    else if current == final then
        [ current ]

    else
        current :: validMonthKeysThrough (Calendar.shiftMonth 1 current) final


bucketFor : List ActivityBucket -> String -> ActivityBucket
bucketFor buckets key =
    buckets
        |> List.filter (.key >> (==) key)
        |> List.head
        |> Maybe.withDefault (emptyBucket key)


emptyBucket : String -> ActivityBucket
emptyBucket key =
    { key = key
    , startDate = monthStart key
    , total = Decimal.zero
    , increase = Decimal.zero
    , decrease = Decimal.zero
    , reallocated = Decimal.zero
    , categories = []
    , directions = []
    , transactions = []
    }


monthStart : String -> Date
monthStart key =
    case Time.monthFromString key of
        Just (Month month) ->
            Date { year = month.year, month = month.month, day = 1 }

        Nothing ->
            Date { year = 1970, month = 1, day = 1 }
