module ActivityCalendarTest exposing (suite)

import Data.ActivityCalendar as ActivityCalendar
import Data.Analytics exposing (ActivityBucket)
import Data.Date exposing (Date(..))
import Data.Decimal as Decimal
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "year activity calendar"
        [ test "shows nothing when the range has no values" <|
            \_ ->
                ActivityCalendar.yearBuckets "2025-04" []
                    |> Expect.equal []
        , test "fills leading and internal empty months through the final value" <|
            \_ ->
                [ bucket "2025-06" "10"
                , bucket "2025-08" "20"
                ]
                    |> ActivityCalendar.yearBuckets "2025-04"
                    |> List.map (\item -> ( item.key, Decimal.toString item.total ))
                    |> Expect.equal
                        [ ( "2025-04", "0" )
                        , ( "2025-05", "0" )
                        , ( "2025-06", "10" )
                        , ( "2025-07", "0" )
                        , ( "2025-08", "20" )
                        ]
        ]


bucket : String -> String -> ActivityBucket
bucket key total =
    { key = key
    , startDate = Date { year = 2025, month = 1, day = 1 }
    , total = Decimal.fromString total |> Result.withDefault Decimal.zero
    , increase = Decimal.zero
    , decrease = Decimal.zero
    , reallocated = Decimal.zero
    , categories = []
    , transactions = []
    }
