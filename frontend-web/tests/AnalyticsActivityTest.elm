module AnalyticsActivityTest exposing (suite)

import Api.Analytics as Api
import Data.Analytics as Analytics exposing (ActivityMovement)
import Data.Decimal as Decimal
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "typed activity responses"
        [ test "asset transfer retains both endpoints" <|
            \_ ->
                decodeMovement "asset" (linked "transferred")
                    |> Result.map
                        (\movement ->
                            case movement of
                                Analytics.AssetMovements figures ->
                                    case figures.events of
                                        [ Analytics.Transferred from outgoing to incoming ] ->
                                            ( from ++ " → " ++ to
                                            , Decimal.toString outgoing
                                            , Decimal.toString incoming
                                            )

                                        _ ->
                                            ( "missing", "", "" )

                                _ ->
                                    ( "wrong kind", "", "" )
                        )
                    |> Expect.equal (Ok ( "assets:bank → assets:wallet", "40", "40" ))
        , test "liability refinance is one linked event" <|
            \_ ->
                decodeMovement "liability" (linked "refinanced")
                    |> Result.map
                        (\movement ->
                            case movement of
                                Analytics.LiabilityMovements figures ->
                                    case figures.events of
                                        [ Analytics.Refinanced old repaid new borrowed ] ->
                                            ( old ++ " → " ++ new
                                            , Decimal.toString repaid
                                            , Decimal.toString borrowed
                                            )

                                        _ ->
                                            ( "missing", "", "" )

                                _ ->
                                    ( "wrong kind", "", "" )
                        )
                    |> Expect.equal
                        (Ok ( "liabilities:old → liabilities:new", "40", "40" ))
        , test "income reversal remains visible when net is zero" <|
            \_ ->
                decodeMovement "income" (simple "income_reversed")
                    |> Result.map
                        (\movement ->
                            case movement of
                                Analytics.IncomeMovements figures ->
                                    ( Decimal.toString figures.posted
                                    , Decimal.toString figures.reversed
                                    , List.length figures.events
                                    )

                                _ ->
                                    ( "wrong kind", "", 0 )
                        )
                    |> Expect.equal (Ok ( "40", "40", 1 ))
        , test "expense reversal is not income" <|
            \_ ->
                decodeMovement "expense" (simple "expense_reversed")
                    |> Result.map
                        (\movement ->
                            case movement of
                                Analytics.ExpenseMovements figures ->
                                    case figures.events of
                                        [ Analytics.FlowReversed _ amount ] ->
                                            Decimal.toString amount

                                        _ ->
                                            "missing"

                                _ ->
                                    "wrong kind"
                        )
                    |> Expect.equal (Ok "40")
        , test "rejects an asset event in a liability bucket" <|
            \_ ->
                decodeMovement "liability" (simple "acquired")
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "rejects reallocation in a liability bucket" <|
            \_ ->
                activityResponse "liability" (simple "borrowed") "reallocate"
                    |> Decode.decodeValue Api.activityDecoder
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]


decodeMovement : String -> Encode.Value -> Result Decode.Error ActivityMovement
decodeMovement kind event =
    activityResponse kind event "increase"
        |> Decode.decodeValue Api.activityDecoder
        |> Result.andThen
            (\response ->
                case response.buckets of
                    firstBucket :: _ ->
                        Ok firstBucket.movement

                    [] ->
                        Err (Decode.Failure "Missing bucket" Encode.null)
            )


activityResponse : String -> Encode.Value -> String -> Encode.Value
activityResponse kind event direction =
    Encode.object
        [ ( "metadata", Encode.object [] )
        , ( "displayCommodity", Encode.string "INR" )
        , ( "kind", Encode.string kind )
        , ( "range", Encode.string "month" )
        , ( "period", Encode.string "2024-04" )
        , ( "fiscalYearStartMonth", Encode.int 4 )
        , ( "warnings", Encode.list identity [] )
        , ( "buckets", Encode.list identity [ bucket event direction ] )
        ]


bucket : Encode.Value -> String -> Encode.Value
bucket event direction =
    Encode.object
        [ ( "key", Encode.string "2024-04-01" )
        , ( "startDate", Encode.string "2024-04-01" )
        , ( "total", Encode.string "0" )
        , ( "increase", Encode.string "40" )
        , ( "decrease", Encode.string "40" )
        , ( "reallocated", Encode.string "40" )
        , ( "borrowed", Encode.string "40" )
        , ( "repaid", Encode.string "40" )
        , ( "forgiven", Encode.string "0" )
        , ( "borrowingReversed", Encode.string "0" )
        , ( "netDebtMovement", Encode.string "0" )
        , ( "posted", Encode.string "40" )
        , ( "reversed", Encode.string "40" )
        , ( "categories", Encode.list identity [] )
        , ( "directions", Encode.list identity [ directionEntry direction ] )
        , ( "events", Encode.list identity [ event ] )
        , ( "transactions", Encode.list identity [] )
        ]


directionEntry : String -> Encode.Value
directionEntry direction =
    Encode.object
        [ ( "direction", Encode.string direction )
        , ( "categories", Encode.list identity [] )
        ]


simple : String -> Encode.Value
simple effect =
    Encode.object
        [ ( "effect", Encode.string effect )
        , ( "account", Encode.string "income:salary" )
        , ( "amount", Encode.string "40" )
        ]


linked : String -> Encode.Value
linked effect =
    let
        ( source, destination ) =
            if effect == "refinanced" then
                ( "liabilities:old", "liabilities:new" )

            else
                ( "assets:bank", "assets:wallet" )
    in
    Encode.object
        [ ( "effect", Encode.string effect )
        , ( "source", Encode.string source )
        , ( "sourceAmount", Encode.string "40" )
        , ( "destination", Encode.string destination )
        , ( "destinationAmount", Encode.string "40" )
        ]
