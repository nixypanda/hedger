module InvestmentsTest exposing (suite)

import Api.Investments as Investments exposing (Granularity(..))
import Data.Completeness exposing (Completeness(..))
import Data.Decimal as Decimal
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
                            , periods = List.length response.timeline
                            , child = response.children |> List.head |> Maybe.map .account
                            , warning = response.warnings |> List.head |> Maybe.map .kind
                            }
                        )
                    |> Expect.equal
                        (Ok
                            { commodity = "INR"
                            , completeness = Partial
                            , gain = Just "3000"
                            , profit = Nothing
                            , periods = 1
                            , child = Just "assets:investments:equity"
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
        , test "rounds investment return percentages" percentageFormatting
        ]


percentageFormatting : () -> Expect.Expectation
percentageFormatting _ =
    Decimal.fromString "91.2198020288977159880834160873882820258192651439920556107249255"
        |> Result.map Formatting.formatPercentage
        |> Expect.equal (Ok "91.22%")


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
    "returnPercentage": "20",
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
    "returnPercentage": "20",
    "attributedIncome": null,
    "realisedGain": null,
    "totalProfit": null,
    "completeness": "partial"
  }],
  "breadcrumbs": [],
  "children": [{
    "account": "assets:investments:equity",
    "label": "Equity",
    "summary": {
      "marketValue": "18000",
      "purchaseCost": "15000",
      "gainLoss": "3000",
      "returnPercentage": "20",
      "attributedIncome": null,
      "realisedGain": null,
      "totalProfit": null,
      "completeness": "partial"
    },
    "hasChildren": true
  }],
  "warnings": [{
    "kind": "missing-performance-mapping",
    "account": "assets:investments:equity",
    "message": "Income and realised gains are not linked."
  }]
}"""
