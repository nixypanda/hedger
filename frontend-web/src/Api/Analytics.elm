module Api.Analytics exposing
    ( ActivityRequest
    , AnalyticsRequest
    , activityDecoder
    , activityRequestEncoder
    , cashflowDecoder
    , netWorthDecoder
    , requestEncoder
    )

import Api.Json exposing (maybeString, required)
import Api.Metadata as Metadata
import Api.Transaction as Transaction
import Data.AccountType as AccountType
import Data.Analytics as AnalyticsData
    exposing
        ( ActivityBucket
        , ActivityDirection
        , ActivityKind(..)
        , ActivityRange(..)
        , ActivityResponse
        , BreakdownItem
        , CashflowResponse
        , CategoryBreakdown
        , DirectionBreakdown
        , Kpi
        , NetWorthResponse
        , PerformancePoint
        , TimelinePoint
        )
import Data.Completeness as Completeness
import Data.Date as Date
import Data.Decimal as Decimal
import Data.Granularity as Granularity exposing (Granularity)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias AnalyticsRequest =
    { query : String
    , granularity : Granularity
    , displayCommodity : Maybe String
    , positivePeriod : Maybe String
    , negativePeriod : Maybe String
    , positiveDepth : Int
    , negativeDepth : Int
    }


type alias ActivityRequest =
    { kind : ActivityKind
    , range : ActivityRange
    , period : String
    , query : String
    , displayCommodity : Maybe String
    , depth : Int
    }


requestEncoder : AnalyticsRequest -> Encode.Value
requestEncoder request =
    Encode.object
        [ ( "query", Encode.string request.query )
        , ( "granularity", Encode.string (Granularity.toString request.granularity) )
        , ( "displayCommodity", maybeString request.displayCommodity )
        , ( "positivePeriod", maybeString request.positivePeriod )
        , ( "negativePeriod", maybeString request.negativePeriod )
        , ( "positiveDepth", Encode.int request.positiveDepth )
        , ( "negativeDepth", Encode.int request.negativeDepth )
        ]


activityRequestEncoder : ActivityRequest -> Encode.Value
activityRequestEncoder request =
    Encode.object
        [ ( "kind", Encode.string (activityKindString request.kind) )
        , ( "range", Encode.string (activityRangeString request.range) )
        , ( "period", Encode.string request.period )
        , ( "query", Encode.string request.query )
        , ( "displayCommodity", maybeString request.displayCommodity )
        , ( "depth", Encode.int request.depth )
        ]


activityKindString : ActivityKind -> String
activityKindString kind =
    case kind of
        IncomeActivity ->
            "income"

        ExpenseActivity ->
            "expense"

        AssetActivity ->
            "asset"

        LiabilityActivity ->
            "liability"


activityRangeString : ActivityRange -> String
activityRangeString range =
    case range of
        MonthActivity ->
            "month"

        YearActivity ->
            "year"


netWorthDecoder : Decoder NetWorthResponse
netWorthDecoder =
    Decode.succeed NetWorthResponse
        |> required "metadata" Metadata.decoder
        |> required "displayCommodity" Decode.string
        |> required "granularity" Granularity.decoder
        |> required "netWorth" kpiDecoder
        |> required "assets" kpiDecoder
        |> required "liabilities" kpiDecoder
        |> required "timeline" (Decode.list timelineDecoder)
        |> required "assetsBreakdown" categoryBreakdownDecoder
        |> required "liabilitiesBreakdown" categoryBreakdownDecoder
        |> required "performance" (Decode.list performanceDecoder)
        |> required "incompletePeriods" Decode.int
        |> required "warnings" (Decode.list Decode.string)


cashflowDecoder : Decoder CashflowResponse
cashflowDecoder =
    Decode.succeed CashflowResponse
        |> required "metadata" Metadata.decoder
        |> required "displayCommodity" Decode.string
        |> required "granularity" Granularity.decoder
        |> required "totalCount" Decode.int
        |> required "filteredCount" Decode.int
        |> required "timelineAvailable" Decode.bool
        |> required "timeline" (Decode.list timelineDecoder)
        |> required "incomeBreakdown" categoryBreakdownDecoder
        |> required "expenseBreakdown" categoryBreakdownDecoder
        |> required "warnings" (Decode.list Decode.string)


activityDecoder : Decoder ActivityResponse
activityDecoder =
    Decode.succeed ActivityResponse
        |> required "metadata" Metadata.decoder
        |> required "displayCommodity" Decode.string
        |> required "kind" activityKindDecoder
        |> required "range" activityRangeDecoder
        |> required "period" Decode.string
        |> required "fiscalYearStartMonth" Decode.int
        |> required "buckets" (Decode.list activityBucketDecoder)
        |> required "warnings" (Decode.list Decode.string)


kpiDecoder : Decoder Kpi
kpiDecoder =
    Decode.succeed Kpi
        |> required "value" Decimal.decoder
        |> required "absoluteChange" (Decode.nullable Decimal.decoder)
        |> required "percentageChange" (Decode.nullable Decimal.decoder)
        |> required "trend" (Decode.list Decimal.decoder)


timelineDecoder : Decoder TimelinePoint
timelineDecoder =
    Decode.succeed TimelinePoint
        |> required "period" Decode.string
        |> required "startDate" Date.decoder
        |> required "positive" Decimal.decoder
        |> required "negative" Decimal.decoder
        |> required "net" Decimal.decoder
        |> required "completeness" Completeness.decoder
        |> required "missingCommodities" (Decode.list Decode.string)


categoryBreakdownDecoder : Decoder CategoryBreakdown
categoryBreakdownDecoder =
    Decode.succeed CategoryBreakdown
        |> required "period" Decode.string
        |> required "total" Decimal.decoder
        |> required "items" (Decode.list breakdownDecoder)
        |> required "otherItems" (Decode.list breakdownDecoder)


breakdownDecoder : Decoder BreakdownItem
breakdownDecoder =
    Decode.succeed AnalyticsData.BreakdownItem
        |> required "account" Decode.string
        |> required "accountType" (Decode.nullable AccountType.decoder)
        |> required "quantity" Decimal.decoder


performanceDecoder : Decoder PerformancePoint
performanceDecoder =
    Decode.succeed PerformancePoint
        |> required "period" Decode.string
        |> required "startDate" Date.decoder
        |> required "netInvestment" Decimal.decoder
        |> required "netWorth" Decimal.decoder
        |> required "gainLoss" Decimal.decoder


activityBucketDecoder : Decoder ActivityBucket
activityBucketDecoder =
    Decode.succeed AnalyticsData.ActivityBucket
        |> required "key" Decode.string
        |> required "startDate" Date.decoder
        |> required "total" Decimal.decoder
        |> required "increase" Decimal.decoder
        |> required "decrease" Decimal.decoder
        |> required "reallocated" Decimal.decoder
        |> required "categories" (Decode.list breakdownDecoder)
        |> required "directions" (Decode.list directionBreakdownDecoder)
        |> required "transactions" (Decode.list Transaction.transactionDecoder)


directionBreakdownDecoder : Decoder DirectionBreakdown
directionBreakdownDecoder =
    Decode.succeed AnalyticsData.DirectionBreakdown
        |> required "direction" activityDirectionDecoder
        |> required "categories" (Decode.list breakdownDecoder)


activityDirectionDecoder : Decoder ActivityDirection
activityDirectionDecoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case value of
                    "increase" ->
                        Decode.succeed AnalyticsData.Increase

                    "reallocate" ->
                        Decode.succeed AnalyticsData.Reallocate

                    "decrease" ->
                        Decode.succeed AnalyticsData.Decrease

                    other ->
                        Decode.fail ("Unknown activity direction: " ++ other)
            )


activityKindDecoder : Decoder ActivityKind
activityKindDecoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case value of
                    "income" ->
                        Decode.succeed IncomeActivity

                    "expense" ->
                        Decode.succeed ExpenseActivity

                    "asset" ->
                        Decode.succeed AssetActivity

                    "liability" ->
                        Decode.succeed LiabilityActivity

                    _ ->
                        Decode.fail "Unknown activity kind."
            )


activityRangeDecoder : Decoder ActivityRange
activityRangeDecoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case value of
                    "month" ->
                        Decode.succeed MonthActivity

                    "year" ->
                        Decode.succeed YearActivity

                    _ ->
                        Decode.fail "Unknown activity range."
            )
