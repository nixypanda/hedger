module InvestmentsTest exposing (suite)

import Api.Investments as Investments
import Data.Completeness exposing (Completeness(..))
import Data.Decimal as Decimal
import Data.Granularity exposing (Granularity(..))
import Expect
import Fuzz
import Json.Decode as Decode
import Page.Investments as Page
import Test exposing (Test, describe, fuzz, test)


{-| Only the newest in-flight request may update the Investments view. A
response that arrives for an older request is stale and must be dropped, even if
the user has since changed granularity or scope.
-}
suite : Test
suite =
    describe "Investments"
        [ test "decodes portfolio, timeline, drilldown, and partial attribution" <|
            \_ ->
                Decode.decodeString Investments.decoder fixture
                    |> Result.map
                        (\response ->
                            { commodity = response.displayCommodity
                            , completeness = response.summary.completeness
                            , gain = response.summary.gainLoss |> Maybe.map Decimal.toString
                            , profit = response.summary.totalProfit
                            , xirr = response.summary.xirr |> Maybe.map Decimal.toString
                            , periods = List.length response.timeline
                            , child = response.children |> List.head |> Maybe.map .account
                            , childAssetClass =
                                response.children
                                    |> List.head
                                    |> Maybe.map .assetClass
                            , childAllocation =
                                response.children
                                    |> List.head
                                    |> Maybe.andThen .allocationPercentage
                                    |> Maybe.map Decimal.toString
                            , assetClassTarget =
                                response.assetClassAllocations
                                    |> List.head
                                    |> Maybe.andThen .targetPercentage
                                    |> Maybe.map Decimal.toString
                            , warning = response.warnings |> List.head |> Maybe.map .kind
                            }
                        )
                    |> Expect.equal
                        (Ok
                            { commodity = "INR"
                            , completeness = Partial
                            , gain = Just "3000"
                            , profit = Nothing
                            , xirr = Just "0.2"
                            , periods = 1
                            , child = Just "assets:investments:equity"
                            , childAssetClass = Just Investments.Equity
                            , childAllocation = Just "100"
                            , assetClassTarget = Just "60"
                            , warning = Just "missing-performance-mapping"
                            }
                        )
        , test "granularity and drilldown selections update request state" <|
            \_ ->
                let
                    ( initial, _ ) =
                        Page.init

                    ( yearly, _ ) =
                        Page.update (Page.GranularitySelected Yearly) initial

                    ( drilled, _ ) =
                        Page.update (Page.AccountSelected "assets:investments:equity") yearly

                    ( selected, _ ) =
                        Page.update (Page.PeriodSelected "2026-03") drilled

                    ( portfolio, _ ) =
                        Page.update Page.PortfolioSelected selected
                in
                Expect.equal
                    { granularity = Yearly
                    , account = Just "assets:investments:equity"
                    , period = Just "2026-03"
                    , portfolioAccount = Nothing
                    , portfolioPeriod = Nothing
                    }
                    { granularity = Page.granularity selected
                    , account = Page.accountPrefix selected
                    , period = Page.selectedPeriod selected
                    , portfolioAccount = Page.accountPrefix portfolio
                    , portfolioPeriod = Page.selectedPeriod portfolio
                    }
        , test "chart mode changes without replacing the selected period" <|
            \_ ->
                let
                    ( initial, _ ) =
                        Page.init

                    ( selected, _ ) =
                        Page.update (Page.PeriodSelected "2026-03") initial

                    ( returns, _ ) =
                        Page.update (Page.ChartModeSelected Page.ReturnsChart) selected
                in
                Expect.equal
                    { mode = Page.ReturnsChart, period = Just "2026-03" }
                    { mode = Page.chartMode returns, period = Page.selectedPeriod returns }
        , fuzz investmentsFuzzer
            "a stale response cannot overwrite a newer selection"
            staleResponseHolds
        ]


type ScopeOp
    = ChangeGranularity Granularity
    | SelectAccount String
    | SelectPortfolio


type Completion
    = Stale Int
    | Active


type alias Scenario =
    { ops : List ScopeOp
    , completion : Completion
    , granularity : Granularity
    }


type alias Trace =
    { model : Page.Model
    , issued : Int
    }


investmentsFuzzer : Fuzz.Fuzzer Scenario
investmentsFuzzer =
    Fuzz.map3 Scenario
        (Fuzz.list scopeOpFuzzer)
        (Fuzz.oneOf [ Fuzz.constant Active, Fuzz.map Stale (Fuzz.intRange 1 5) ])
        (Fuzz.oneOfValues [ Monthly, Yearly ])


scopeOpFuzzer : Fuzz.Fuzzer ScopeOp
scopeOpFuzzer =
    Fuzz.oneOf
        [ Fuzz.map ChangeGranularity (Fuzz.oneOfValues [ Monthly, Yearly ])
        , Fuzz.map SelectAccount
            (Fuzz.oneOfValues
                [ "assets:investments:equity"
                , "assets:investments:debt"
                , "assets:investments"
                ]
            )
        , Fuzz.constant SelectPortfolio
        ]


staleResponseHolds : Scenario -> Expect.Expectation
staleResponseHolds scenario =
    let
        trace =
            List.foldl stepScope initialTrace scenario.ops

        before =
            snapshot trace.model

        token =
            case scenario.completion of
                Active ->
                    trace.issued

                Stale age ->
                    trace.issued - age
    in
    case responseWith scenario.granularity of
        Nothing ->
            Expect.fail "The Investments fixture did not decode."

        Just response ->
            let
                after =
                    Page.update (Page.ResponseLoaded token (Ok response)) trace.model
                        |> Tuple.first
            in
            case scenario.completion of
                Stale _ ->
                    snapshot after
                        |> Expect.equal before

                Active ->
                    Page.granularity after
                        |> Expect.equal scenario.granularity


initialTrace : Trace
initialTrace =
    { model = Page.init |> Tuple.first, issued = 1 }


stepScope : ScopeOp -> Trace -> Trace
stepScope op trace =
    case op of
        ChangeGranularity granularity ->
            { trace
                | model = applyGranularity granularity trace.model
                , issued = trace.issued + 1
            }

        SelectAccount account ->
            { trace
                | model = Page.update (Page.AccountSelected account) trace.model |> Tuple.first
                , issued = trace.issued + 1
            }

        SelectPortfolio ->
            { trace
                | model = Page.update Page.PortfolioSelected trace.model |> Tuple.first
                , issued = trace.issued + 1
            }


type alias InvestmentsSnapshot =
    { granularity : Granularity
    , account : Maybe String
    , period : Maybe String
    , mode : Page.ChartMode
    }


snapshot : Page.Model -> InvestmentsSnapshot
snapshot model =
    { granularity = Page.granularity model
    , account = Page.accountPrefix model
    , period = Page.selectedPeriod model
    , mode = Page.chartMode model
    }


applyGranularity : Granularity -> Page.Model -> Page.Model
applyGranularity granularity model =
    Page.update (Page.GranularitySelected granularity) model |> Tuple.first


responseWith : Granularity -> Maybe Investments.Response
responseWith granularity =
    case Decode.decodeString Investments.decoder fixture of
        Ok response ->
            Just { response | granularity = granularity }

        Err _ ->
            Nothing


fixture : String
fixture =
    """{
  "metadata": { "revision": "r1", "freshness": "partial", "warnings": [] },
  "displayCommodity": "INR",
  "granularity": "monthly",
  "accountPrefix": null,
  "summary": {
    "marketValue": "18000",
    "purchaseCost": "15000",
    "gainLoss": "3000",
    "xirr": "0.2",
    "attributedIncome": null,
    "realisedGain": null,
    "totalProfit": null,
    "completeness": "partial"
  },
  "timeline": [{
    "period": "2026-03",
    "startDate": "2026-03-01",
    "marketValue": "18000",
    "purchaseCost": "15000",
    "gainLoss": "3000",
    "xirr": "0.2",
    "attributedIncome": null,
    "realisedGain": null,
    "totalProfit": null,
    "completeness": "partial"
  }],
  "breadcrumbs": [],
  "assetClassAllocations": [{
    "assetClass": "equity",
    "marketValue": "18000",
    "currentPercentage": "100",
    "targetPercentage": "60"
  }],
  "children": [{
    "account": "assets:investments:equity",
    "label": "Equity",
    "summary": {
      "marketValue": "18000",
      "purchaseCost": "15000",
      "gainLoss": "3000",
      "xirr": "0.2",
      "attributedIncome": null,
      "realisedGain": null,
      "totalProfit": null,
      "completeness": "partial"
    },
    "hasChildren": true,
    "assetClass": "equity",
    "allocationPercentage": "100"
  }],
  "warnings": [{
    "kind": "missing-performance-mapping",
    "account": "assets:investments:equity",
    "message": "Income and realised gains are not linked."
  }]
}"""
