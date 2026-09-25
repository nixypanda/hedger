module RegisterTest exposing (suite)

import Api.Register as Register
import Data.Completeness as Completeness
import Data.Freshness as Freshness
import Expect
import Fuzz
import Json.Decode as Decode
import Page.AccountRegister as Page
import Test exposing (Test, describe, fuzz, test)


{-| A register page may only be replaced or appended by the request that is
currently in flight. Completions for earlier requests are stale and must leave
the visible register untouched.
-}
suite : Test
suite =
    describe "Account register"
        [ test "decodes converted, partial, and multi-posting register data" <|
            \_ ->
                Decode.decodeString Register.decoder fixture
                    |> Result.map
                        (\response ->
                            { commodity = response.displayCommodity
                            , entryCount = List.length response.entries
                            , completeness =
                                response.entries
                                    |> List.drop 1
                                    |> List.head
                                    |> Maybe.map (.balance >> .completeness)
                            , nextCursor = response.nextCursor
                            }
                        )
                    |> Expect.equal
                        (Ok
                            { commodity = "INR"
                            , entryCount = 2
                            , completeness = Just Completeness.Partial
                            , nextCursor = Just "page-2"
                            }
                        )
        , fuzz accountFuzzer
            "only the active request can replace the register"
            activeRequestWins
        ]


accountFuzzer : Fuzz.Fuzzer (List String)
accountFuzzer =
    Fuzz.list
        (Fuzz.oneOfValues
            [ "Assets:Cash"
            , "Assets:Bank"
            , "Expenses:Travel"
            ]
        )


activeRequestWins : List String -> Expect.Expectation
activeRequestWins accounts =
    let
        trace =
            List.foldl issueAccount initialTrace ("Assets:Cash" :: accounts)

        issued =
            trace.issued

        lastIndex =
            List.length issued - 1

        emptyResponse =
            { sampleResponse | entries = [] }

        afterStale =
            List.range 0 (lastIndex - 1)
                |> List.foldl
                    (\index model -> applyResponse issued index sampleResponse model)
                    trace.model

        activeResponse =
            applyResponse issued lastIndex sampleResponse afterStale

        replacedResponse =
            applyResponse issued lastIndex emptyResponse afterStale
    in
    Expect.all
        [ \_ -> Expect.equal (Page.visibleResponse trace.model) (Page.visibleResponse afterStale)
        , \_ -> Expect.equal (Just sampleResponse) (Page.visibleResponse activeResponse)
        , \_ -> Expect.equal (Just emptyResponse) (Page.visibleResponse replacedResponse)
        ]
        ()


applyResponse : List Page.Request -> Int -> Register.Response -> Page.Model -> Page.Model
applyResponse issued index response model =
    Page.receive (requestAt index issued) (Ok response) model


type alias Trace =
    { model : Page.Model
    , issued : List Page.Request
    }


initialTrace : Trace
initialTrace =
    { model = Page.init, issued = [] }


issueAccount : String -> Trace -> Trace
issueAccount account trace =
    let
        ( nextModel, request ) =
            Page.beginLoading account trace.model
    in
    { model = nextModel, issued = trace.issued ++ [ request ] }


requestAt : Int -> List Page.Request -> Page.Request
requestAt index issued =
    issued
        |> List.drop index
        |> List.head
        |> Maybe.withDefault fallbackRequest


fallbackRequest : Page.Request
fallbackRequest =
    Page.beginLoading "Assets:Cash" Page.init |> Tuple.second


sampleResponse : Register.Response
sampleResponse =
    { metadata = { revision = Just "revision-a", freshness = Freshness.Fresh, warnings = [] }
    , displayCommodity = "INR"
    , entries = []
    , nextCursor = Nothing
    }


fixture : String
fixture =
    """{
  "metadata": {
    "revision": "revision-a",
    "freshness": "fresh",
    "warnings": []
  },
  "displayCommodity": "INR",
  "entries": [
    {
      "date": "2025-01-01",
      "description": "Salary",
      "account": "Assets:Cash",
      "amount": {
        "raw": [ { "commodity": "INR", "quantity": "100" } ],
        "display": [ { "commodity": "INR", "quantity": "100" } ],
        "completeness": "complete"
      },
      "balance": {
        "raw": [ { "commodity": "INR", "quantity": "100" } ],
        "display": [ { "commodity": "INR", "quantity": "100" } ],
        "completeness": "complete"
      }
    },
    {
      "date": "2025-01-02",
      "description": "Converted USD purchase",
      "account": "Assets:Cash, Expenses:Travel",
      "amount": {
        "raw": [ { "commodity": "USD", "quantity": "-10" } ],
        "display": [ { "commodity": "INR", "quantity": "-830" } ],
        "completeness": "complete"
      },
      "balance": {
        "raw": [
          { "commodity": "INR", "quantity": "100" },
          { "commodity": "JPY", "quantity": "1000" }
        ],
        "display": [
          { "commodity": "INR", "quantity": "-730" },
          { "commodity": "JPY", "quantity": "1000" }
        ],
        "completeness": "partial"
      }
    }
  ],
  "nextCursor": "page-2"
}"""
