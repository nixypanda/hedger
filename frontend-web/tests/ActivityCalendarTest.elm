module ActivityCalendarTest exposing (suite)

import Api.Analytics as Analytics exposing (ActivityKind(..), ActivityRange(..))
import Api.Metadata as Metadata
import Data.ActivityCalendar as ActivityCalendar
import Data.Date exposing (Date(..))
import Data.Decimal as Decimal
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "year activity calendar"
        [ test "shows nothing when the range has no values" <|
            \_ ->
                emptyResponse
                    |> ActivityCalendar.yearBuckets
                    |> Expect.equal []
        , test "fills leading and internal empty months through the final value" <|
            \_ ->
                { emptyResponse
                    | buckets =
                        [ bucket "2025-06" "10"
                        , bucket "2025-08" "20"
                        ]
                }
                    |> ActivityCalendar.yearBuckets
                    |> List.map (\item -> ( item.key, Decimal.toString item.total ))
                    |> Expect.equal
                        [ ( "2025-04", "0" )
                        , ( "2025-05", "0" )
                        , ( "2025-06", "10" )
                        , ( "2025-07", "0" )
                        , ( "2025-08", "20" )
                        ]
        ]


emptyResponse : Analytics.ActivityResponse
emptyResponse =
    { metadata = Metadata.empty
    , displayCommodity = "INR"
    , kind = ExpenseActivity
    , range = YearActivity
    , period = "2025-04"
    , fiscalYearStartMonth = 4
    , buckets = []
    , warnings = []
    }


bucket : String -> String -> Analytics.ActivityBucket
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
