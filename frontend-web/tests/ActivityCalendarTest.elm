module ActivityCalendarTest exposing (suite)

import Data.ActivityCalendar as ActivityCalendar
import Data.Analytics exposing (ActivityBucket, ActivityKind(..), ActivityMovement(..))
import Data.Calendar as Calendar
import Data.Date exposing (Date(..))
import Data.Decimal as Decimal
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz, test)


{-| The year activity view must present a contiguous month series from the
requested start through the latest month that has activity, zero-filling the
months in between. This is the domain rule the Activity calendar exists to
enforce: a gap in the ledger is a real negligible month, not a missing column.
-}
suite : Test
suite =
    describe "year activity calendar"
        [ fuzz monthRangeFuzzer "shows the contiguous month range through the final bucket" <|
            \input ->
                let
                    buckets =
                        List.map (bucketAt input) input.offsets
                in
                ActivityCalendar.yearBuckets IncomeActivity (startKey input) buckets
                    |> List.map (\item -> ( item.key, Decimal.toString item.total ))
                    |> Expect.equal (expectedBuckets input)
        , test "fills gaps with the requested movement kind" <|
            \_ ->
                [ AssetActivity, LiabilityActivity, IncomeActivity, ExpenseActivity ]
                    |> List.map
                        (\kind ->
                            ActivityCalendar.yearBuckets kind "2024-04" [ bucketAt sampleInput 1 ]
                                |> List.head
                                |> Maybe.map (.movement >> movementKind)
                        )
                    |> Expect.equal
                        [ Just AssetActivity
                        , Just LiabilityActivity
                        , Just IncomeActivity
                        , Just ExpenseActivity
                        ]
        , test "month grids contain only their real week rows" <|
            \_ -> monthGridsUseRealWeekRows ()
        , test "December 2025 uses five real week rows" <|
            \_ ->
                Calendar.monthWeeks "2025-12"
                    |> List.length
                    |> Expect.equal 5
        , test "February 2015 uses four real week rows" <|
            \_ ->
                Calendar.monthWeeks "2015-02"
                    |> List.length
                    |> Expect.equal 4
        ]


monthGridsUseRealWeekRows : () -> Expect.Expectation
monthGridsUseRealWeekRows _ =
    [ 2024, 2025 ]
        |> List.concatMap monthsOfYear
        |> List.map monthGridSummary
        |> List.all realWeekRows
        |> Expect.equal True


monthGridSummary :
    String
    ->
        { rows : Int
        , widths : List Int
        , hasEmptyRow : Bool
        , cells : Int
        }
monthGridSummary period =
    let
        weeks =
            Calendar.monthWeeks period
    in
    { rows = List.length weeks
    , widths = List.map List.length weeks
    , hasEmptyRow = List.any (List.all ((==) Nothing)) weeks
    , cells = List.length (Calendar.monthCells period)
    }


realWeekRows :
    { rows : Int
    , widths : List Int
    , hasEmptyRow : Bool
    , cells : Int
    }
    -> Bool
realWeekRows summary =
    summary.rows
        >= 4
        && summary.rows
        <= 6
        && List.all ((==) 7) summary.widths
        && not summary.hasEmptyRow
        && summary.cells
        == 7
        * summary.rows


monthsOfYear : Int -> List String
monthsOfYear year =
    List.range 1 12
        |> List.map
            (\month ->
                String.fromInt year
                    ++ "-"
                    ++ String.padLeft 2 '0' (String.fromInt month)
            )


sampleInput : Input
sampleInput =
    { year = 2024, month = 4, offsets = [ 1 ] }


movementKind : ActivityMovement -> ActivityKind
movementKind movement =
    case movement of
        AssetMovements _ ->
            AssetActivity

        LiabilityMovements _ ->
            LiabilityActivity

        IncomeMovements _ ->
            IncomeActivity

        ExpenseMovements _ ->
            ExpenseActivity


type alias Input =
    { year : Int
    , month : Int
    , offsets : List Int
    }


monthRangeFuzzer : Fuzz.Fuzzer Input
monthRangeFuzzer =
    Fuzz.map3 Input
        (Fuzz.intRange 1990 2100)
        (Fuzz.intRange 1 12)
        (Fuzz.list (Fuzz.intRange 0 18))


startKey : Input -> String
startKey input =
    monthKey ( input.year, input.month )


expectedBuckets : Input -> List ( String, String )
expectedBuckets input =
    case List.maximum input.offsets of
        Nothing ->
            []

        Just finalOffset ->
            List.range 0 finalOffset
                |> List.map
                    (\offset ->
                        let
                            key =
                                keyAt input offset
                        in
                        ( key, totalAt input key )
                    )


totalAt : Input -> String -> String
totalAt input key =
    input.offsets
        |> List.filter (\offset -> keyAt input offset == key)
        |> List.head
        |> Maybe.map (\offset -> String.fromInt (totalFor offset))
        |> Maybe.withDefault "0"


bucketAt : Input -> Int -> ActivityBucket
bucketAt input offset =
    { key = keyAt input offset
    , startDate = Date { year = input.year, month = input.month, day = 1 }
    , total =
        Decimal.fromString (String.fromInt (totalFor offset))
            |> Result.withDefault Decimal.zero
    , categories = []
    , movement =
        IncomeMovements { posted = Decimal.zero, reversed = Decimal.zero, events = [] }
    , transactions = []
    }


totalFor : Int -> Int
totalFor offset =
    (offset + 1) * 10


keyAt : Input -> Int -> String
keyAt input offset =
    monthKeyTuple (addMonths offset ( input.year, input.month ))


monthKey : ( Int, Int ) -> String
monthKey =
    monthKeyTuple


monthKeyTuple : ( Int, Int ) -> String
monthKeyTuple ( year, month ) =
    String.fromInt year ++ "-" ++ String.padLeft 2 '0' (String.fromInt month)


{-| Independent month arithmetic so the expectation does not lean on
`Data.Calendar.shiftMonth`, which `yearBuckets` itself uses.
-}
addMonths : Int -> ( Int, Int ) -> ( Int, Int )
addMonths offset ( year, month ) =
    let
        zeroBased =
            year * 12 + (month - 1) + offset
    in
    ( zeroBased // 12, modBy 12 zeroBased + 1 )
