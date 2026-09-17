module InvestmentsTest exposing (suite)

import Api.Investments as Investments
import Data.Completeness exposing (Completeness(..))
import Data.Decimal as Decimal
import Data.Granularity exposing (Granularity(..))
import Expect
import Json.Decode as Decode
import Page.Investments as Page
import Test exposing (Test, describe, test)
import View.Formatting as Formatting


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
        , test "formats annual XIRR fractions as percentages" percentageFormatting
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
        , test "a stale response cannot overwrite a newer selection" staleResponseTest
        ]


staleResponseTest : () -> Expect.Expectation
staleResponseTest _ =
    case Decode.decodeString Investments.decoder fixture of
        Err error ->
            Expect.fail (Decode.errorToString error)

        Ok monthlyResponse ->
            let
                ( initial, _ ) =
                    Page.init

                ( yearly, _ ) =
                    Page.update (Page.GranularitySelected Yearly) initial

                yearlyResponse =
                    { monthlyResponse | granularity = Yearly }

                ( loaded, _ ) =
                    Page.update (Page.ResponseLoaded 2 (Ok yearlyResponse)) yearly

                ( afterStale, _ ) =
                    Page.update (Page.ResponseLoaded 1 (Ok monthlyResponse)) loaded
            in
            Expect.equal Yearly (Page.granularity afterStale)


percentageFormatting : () -> Expect.Expectation
percentageFormatting _ =
    Decimal.fromString "0.07"
        |> Result.map Formatting.formatRatePercentage
        |> Expect.equal (Ok "7%")


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
