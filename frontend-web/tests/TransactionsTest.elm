module TransactionsTest exposing (suite)

import Api.Error exposing (ApiError)
import Api.Metadata exposing (Freshness(..), Metadata)
import Api.Transaction as Transaction exposing (Response)
import Expect
import Json.Decode as Decode
import Page.Transactions as Page
import RemoteData exposing (RemoteData(..))
import Test exposing (Test, describe, test)
import TransactionGolden


suite : Test
suite =
    describe "Phase 4A transactions"
        [ test "decodes the backend transaction golden without losing nested postings or costs" <|
            \_ ->
                Decode.decodeString Transaction.decoder TransactionGolden.json
                    |> Result.map
                        (\response ->
                            ( List.length response.transactions
                            , response.transactions
                                |> List.concatMap .postings
                                |> List.length
                            , response.transactions
                                |> List.concatMap .postings
                                |> List.concatMap .amounts
                                |> List.filterMap .cost
                                |> List.map .costType
                            )
                        )
                    |> Expect.equal (Ok ( 13, 27, [ "total", "unit" ] ))
        , test "initial load has no stale data" <|
            \_ ->
                Page.beginLoading Page.init
                    |> .response
                    |> Expect.equal (Loading Nothing)
        , test "selection uses the complete already-loaded DTO" <|
            \_ ->
                let
                    response =
                        sampleResponse "revision-a"

                    model =
                        Page.receive (Ok response) Page.init
                in
                case response.transactions of
                    transaction :: _ ->
                        Page.select transaction model
                            |> .selected
                            |> Maybe.map .postings
                            |> Maybe.map List.length
                            |> Expect.equal (Just 1)

                    [] ->
                        Expect.fail "The test response needs one transaction."
        , test "refresh preserves stale data and selection" <|
            \_ ->
                let
                    response =
                        sampleResponse "revision-a"

                    selected =
                        selectFirst response (Page.receive (Ok response) Page.init)
                in
                Page.beginLoading selected
                    |> (\model -> ( remoteDataValue model.response, model.selected ))
                    |> Expect.equal ( Just response, selected.selected )
        , test "same-revision success retains selection" <|
            \_ ->
                let
                    response =
                        sampleResponse "revision-a"

                    selected =
                        selectFirst response (Page.receive (Ok response) Page.init)
                in
                Page.receive (Ok response) selected
                    |> .selected
                    |> Expect.equal selected.selected
        , test "revision change clears selection" <|
            \_ ->
                let
                    first =
                        sampleResponse "revision-a"

                    selected =
                        selectFirst first (Page.receive (Ok first) Page.init)
                in
                Page.receive (Ok (sampleResponse "revision-b")) selected
                    |> .selected
                    |> Expect.equal Nothing
        , test "an empty response reaches success without manufacturing data" <|
            \_ ->
                Page.receive
                    (Ok { metadata = metadata "revision-a", transactions = [] })
                    Page.init
                    |> .response
                    |> remoteDataValue
                    |> Maybe.map .transactions
                    |> Expect.equal (Just [])
        , test "refresh failure retains stale response and selection" <|
            \_ ->
                let
                    response =
                        sampleResponse "revision-a"

                    selected =
                        selectFirst response (Page.receive (Ok response) Page.init)

                    failed =
                        Page.receive (Err apiError) selected
                in
                ( remoteDataValue failed.response, failed.selected )
                    |> Expect.equal ( Just response, selected.selected )
        , test "fatal failure has no stale response" <|
            \_ ->
                Page.receive (Err apiError) Page.init
                    |> .response
                    |> remoteDataValue
                    |> Expect.equal Nothing
        ]


remoteDataValue : RemoteData value -> Maybe value
remoteDataValue remote =
    case remote of
        NotAsked ->
            Nothing

        Loading stale ->
            stale

        Success value ->
            Just value

        Failure _ stale ->
            stale


selectFirst : Response -> Page.Model -> Page.Model
selectFirst response model =
    case response.transactions of
        transaction :: _ ->
            Page.select transaction model

        [] ->
            model


sampleResponse : String -> Response
sampleResponse revision =
    { metadata = metadata revision
    , transactions =
        [ { primaryDate = "2024-04-02"
          , secondaryDate = Nothing
          , status = "unmarked"
          , code = Nothing
          , description = "Utilities"
          , comment = Nothing
          , tags = []
          , postings =
                [ { account = "expenses:utilities"
                  , status = "unmarked"
                  , comment = Nothing
                  , tags = []
                  , amounts = []
                  }
                ]
          , presentation =
                { classification = "expense"
                , fromAccounts = [ "assets" ]
                , toAccounts = [ "expenses" ]
                , totalAmounts = []
                , openingAccountCount = Nothing
                }
          }
        ]
    }


metadata : String -> Metadata
metadata revision =
    { revision = Just revision, freshness = Fresh, warnings = [] }


apiError : ApiError
apiError =
    { code = "network_error", message = "Unavailable", requestId = Nothing }
