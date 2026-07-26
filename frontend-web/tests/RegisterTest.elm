module RegisterTest exposing (suite)

import Api.Metadata as Metadata
import Api.Register as Register
import Data.Completeness as Completeness
import Expect
import Json.Decode as Decode
import Page.AccountRegister as Page
import Test exposing (Test, describe, test)


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
        , test "only the active page request can replace or append entries" <|
            \_ ->
                let
                    ( loading, first ) =
                        Page.beginLoading "Assets:Cash" Page.init

                    loaded =
                        Page.receive first (Ok sampleResponse) loading

                    ( refreshing, current ) =
                        Page.beginLoading "Assets:Cash" loaded
                in
                Page.receive first (Ok sampleResponse) refreshing
                    |> Page.receive current (Ok { sampleResponse | entries = [] })
                    |> Page.visibleResponse
                    |> (\response ->
                            case response of
                                Just visible ->
                                    Expect.equal [] visible.entries

                                Nothing ->
                                    Expect.fail "Expected the active response to win."
                       )
        ]


sampleResponse : Register.Response
sampleResponse =
    { metadata = { revision = Just "revision-a", freshness = Metadata.Fresh, warnings = [] }
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
