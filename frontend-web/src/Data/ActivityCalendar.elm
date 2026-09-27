module Data.ActivityCalendar exposing (yearBuckets)

import Data.Analytics exposing (ActivityBucket, ActivityKind(..), ActivityMovement(..))
import Data.Calendar as Calendar
import Data.Date exposing (Date(..))
import Data.Decimal as Decimal
import Data.Time as Time exposing (Month(..))


yearBuckets : ActivityKind -> String -> List ActivityBucket -> List ActivityBucket
yearBuckets kind period buckets =
    case latestBucket buckets of
        Nothing ->
            []

        Just finalBucket ->
            monthKeysThrough period finalBucket.key
                |> List.map (bucketFor kind buckets)


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


bucketFor : ActivityKind -> List ActivityBucket -> String -> ActivityBucket
bucketFor kind buckets key =
    buckets
        |> List.filter (.key >> (==) key)
        |> List.head
        |> Maybe.withDefault (emptyBucket kind key)


emptyBucket : ActivityKind -> String -> ActivityBucket
emptyBucket kind key =
    { key = key
    , startDate = monthStart key
    , total = Decimal.zero
    , categories = []
    , movement = emptyMovement kind
    , transactions = []
    }


emptyMovement : ActivityKind -> ActivityMovement
emptyMovement kind =
    case kind of
        AssetActivity ->
            AssetMovements
                { acquired = Decimal.zero
                , disposed = Decimal.zero
                , transferred = Decimal.zero
                , directions = []
                , events = []
                }

        LiabilityActivity ->
            LiabilityMovements
                { borrowed = Decimal.zero
                , repaid = Decimal.zero
                , forgiven = Decimal.zero
                , borrowingReversed = Decimal.zero
                , netDebtMovement = Decimal.zero
                , directions = []
                , events = []
                }

        IncomeActivity ->
            IncomeMovements { posted = Decimal.zero, reversed = Decimal.zero, events = [] }

        ExpenseActivity ->
            ExpenseMovements { posted = Decimal.zero, reversed = Decimal.zero, events = [] }


monthStart : String -> Date
monthStart key =
    case Time.monthFromString key of
        Just (Month month) ->
            Date { year = month.year, month = month.month, day = 1 }

        Nothing ->
            Date { year = 1970, month = 1, day = 1 }
