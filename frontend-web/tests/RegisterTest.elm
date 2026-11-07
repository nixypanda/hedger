module RegisterTest exposing (suite)

import Api.Register as Register
import Data.Completeness as Completeness
import Data.Date as Date
import Data.Freshness as Freshness
import Element
import Expect
import Fuzz
import Json.Decode as Decode
import Page.AccountRegister as Page
import Test exposing (Test, describe, fuzz, test)
import Time
import Ui.Env exposing (Env)
import Ui.Theme as Theme


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
        , test "mobile detail stays open until the pop timeline arrives" <|
            \_ ->
                let
                    loaded =
                        loadRegister { sampleResponse | entries = [ fixtureEntry ] }

                    ( opened, _, _ ) =
                        Page.update
                            { env = mobileEnv }
                            (Page.SelectDetail fixtureEntry)
                            loaded

                    ( openedFrame, _, _ ) =
                        Page.update
                            { env = mobileEnv }
                            (Page.AnimationTick (Time.millisToPosix 0))
                            opened

                    ( openedAtRest, _, _ ) =
                        Page.update
                            { env = mobileEnv }
                            (Page.AnimationTick (Time.millisToPosix 500))
                            openedFrame

                    ( closing, _, _ ) =
                        Page.update { env = mobileEnv } Page.CloseDetail openedAtRest

                    ( closingFrame, _, _ ) =
                        Page.update
                            { env = mobileEnv }
                            (Page.AnimationTick (Time.millisToPosix 1000))
                            closing

                    ( arrived, _, _ ) =
                        Page.update
                            { env = mobileEnv }
                            (Page.AnimationTick (Time.millisToPosix 2000))
                            closingFrame
                in
                Expect.equal
                    { openSelection = Page.selectedEntry opened
                    , openAnimating = Page.isAnimating opened
                    , closingSelection = Page.selectedEntry closing
                    , closingAnimating = Page.isAnimating closing
                    , arrivedSelection = Page.selectedEntry arrived
                    , arrivedAnimating = Page.isAnimating arrived
                    }
                    { openSelection = Just fixtureEntry
                    , openAnimating = True
                    , closingSelection = Just fixtureEntry
                    , closingAnimating = True
                    , arrivedSelection = Nothing
                    , arrivedAnimating = False
                    }
        , test "reduced motion clears the register detail without a transition" <|
            \_ ->
                let
                    loaded =
                        loadRegister { sampleResponse | entries = [ fixtureEntry ] }

                    ( opened, _, _ ) =
                        Page.update
                            { env = reducedMotionEnv }
                            (Page.SelectDetail fixtureEntry)
                            loaded

                    ( closed, _, _ ) =
                        Page.update { env = reducedMotionEnv } Page.CloseDetail opened
                in
                Expect.equal
                    { openSelection = Page.selectedEntry opened
                    , openAnimating = Page.isAnimating opened
                    , closedSelection = Page.selectedEntry closed
                    , closedAnimating = Page.isAnimating closed
                    }
                    { openSelection = Just fixtureEntry
                    , openAnimating = False
                    , closedSelection = Nothing
                    , closedAnimating = False
                    }
        , test "wide layout clears the register detail without a transition" <|
            \_ ->
                let
                    loaded =
                        loadRegister { sampleResponse | entries = [ fixtureEntry ] }

                    ( opened, _, _ ) =
                        Page.update
                            { env = desktopEnv }
                            (Page.SelectDetail fixtureEntry)
                            loaded

                    ( closed, _, _ ) =
                        Page.update { env = desktopEnv } Page.CloseDetail opened
                in
                Expect.equal
                    { opened = Page.selectedEntry opened
                    , openAnimating = Page.isAnimating opened
                    , closed = Page.selectedEntry closed
                    , closedAnimating = Page.isAnimating closed
                    }
                    { opened = Just fixtureEntry
                    , openAnimating = False
                    , closed = Nothing
                    , closedAnimating = False
                    }
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


loadRegister : Register.Response -> Page.Model
loadRegister response =
    let
        ( loading, request ) =
            Page.beginLoading "Assets:Cash" Page.init
    in
    Page.receive request (Ok response) loading


fixtureEntry : Register.Entry
fixtureEntry =
    case fixtureEntries of
        entry :: _ ->
            entry

        [] ->
            { date = Date.Date { year = 2025, month = 1, day = 1 }
            , description = "Salary"
            , account = "Assets:Cash"
            , amount = emptyValue
            , balance = emptyValue
            }


fixtureEntries : List Register.Entry
fixtureEntries =
    Decode.decodeString Register.decoder fixture
        |> Result.map .entries
        |> Result.withDefault []


emptyValue : Register.Value
emptyValue =
    { raw = []
    , display = []
    , completeness = Completeness.Complete
    }


envWith : Int -> Bool -> Env
envWith width reducedMotion =
    let
        viewport =
            { width = width, height = 900 }
    in
    { theme = Theme.forMode Theme.Light
    , device = Element.classifyDevice viewport
    , viewport = viewport
    , reducedMotion = reducedMotion
    }


mobileEnv : Env
mobileEnv =
    envWith 520 False


desktopEnv : Env
desktopEnv =
    envWith 1600 False


reducedMotionEnv : Env
reducedMotionEnv =
    envWith 520 True


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
