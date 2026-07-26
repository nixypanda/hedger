module Api.Analytics exposing
    ( ActivityBucket
    , ActivityKind(..)
    , ActivityRange(..)
    , ActivityRequest
    , ActivityResponse
    , AnalyticsRequest
    , BreakdownItem
    , CashflowResponse
    , CategoryBreakdown
    , Granularity(..)
    , Kpi
    , NetWorthResponse
    , PerformancePoint
    , Report
    , Timeline(..)
    , TimelinePoint
    , activityDecoder
    , activityRequestEncoder
    , cashflowDecoder
    , netWorthDecoder
    , requestEncoder
    )

import Api.Json exposing (maybeString, required)
import Api.Metadata as Metadata exposing (Metadata)
import Api.Transaction as Transaction exposing (Transaction)
import Data.Date as Date exposing (Date)
import Data.Decimal as Decimal exposing (Decimal)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Granularity
    = Monthly
    | Yearly


type ActivityKind
    = IncomeActivity
    | ExpenseActivity
    | AssetActivity
    | LiabilityActivity


type ActivityRange
    = MonthActivity
    | YearActivity


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


type alias Kpi =
    { value : Decimal
    , absoluteChange : Maybe Decimal
    , percentageChange : Maybe Decimal
    , trend : List Decimal
    }


type alias TimelinePoint =
    { period : String
    , startDate : Date
    , positive : Decimal
    , negative : Decimal
    , net : Decimal
    }


type alias BreakdownItem =
    { account : String
    , quantity : Decimal
    }


type alias CategoryBreakdown =
    { period : String
    , total : Decimal
    , items : List BreakdownItem
    , otherItems : List BreakdownItem
    }


type alias PerformancePoint =
    { period : String
    , startDate : Date
    , netInvestment : Decimal
    , netWorth : Decimal
    , gainLoss : Decimal
    }


type alias NetWorthResponse =
    { metadata : Metadata
    , displayCommodity : String
    , granularity : Granularity
    , netWorth : Kpi
    , assets : Kpi
    , liabilities : Kpi
    , timeline : List TimelinePoint
    , assetsBreakdown : CategoryBreakdown
    , liabilitiesBreakdown : CategoryBreakdown
    , performance : List PerformancePoint
    , incompletePeriods : Int
    , warnings : List String
    }


type alias CashflowResponse =
    { metadata : Metadata
    , displayCommodity : String
    , granularity : Granularity
    , totalCount : Int
    , filteredCount : Int
    , timelineAvailable : Bool
    , timeline : List TimelinePoint
    , incomeBreakdown : CategoryBreakdown
    , expenseBreakdown : CategoryBreakdown
    , warnings : List String
    }


type alias ActivityBucket =
    { key : String
    , startDate : Date
    , total : Decimal
    , increase : Decimal
    , decrease : Decimal
    , reallocated : Decimal
    , categories : List BreakdownItem
    , transactions : List Transaction
    }


type alias ActivityResponse =
    { metadata : Metadata
    , displayCommodity : String
    , kind : ActivityKind
    , range : ActivityRange
    , period : String
    , fiscalYearStartMonth : Int
    , buckets : List ActivityBucket
    , warnings : List String
    }


type Timeline
    = TimelineAvailable (List TimelinePoint)
    | TimelineUnavailable String (List TimelinePoint)


type alias Report =
    { displayCommodity : String
    , timeline : Timeline
    , positiveBreakdown : CategoryBreakdown
    , negativeBreakdown : CategoryBreakdown
    , warnings : List String
    }


requestEncoder : AnalyticsRequest -> Encode.Value
requestEncoder request =
    Encode.object
        [ ( "query", Encode.string request.query )
        , ( "granularity", Encode.string (granularityString request.granularity) )
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


granularityString : Granularity -> String
granularityString granularity =
    case granularity of
        Monthly ->
            "monthly"

        Yearly ->
            "yearly"


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
        |> required "granularity" granularityDecoder
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
        |> required "granularity" granularityDecoder
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


categoryBreakdownDecoder : Decoder CategoryBreakdown
categoryBreakdownDecoder =
    Decode.succeed CategoryBreakdown
        |> required "period" Decode.string
        |> required "total" Decimal.decoder
        |> required "items" (Decode.list breakdownDecoder)
        |> required "otherItems" (Decode.list breakdownDecoder)


breakdownDecoder : Decoder BreakdownItem
breakdownDecoder =
    Decode.succeed BreakdownItem
        |> required "account" Decode.string
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
    Decode.succeed ActivityBucket
        |> required "key" Decode.string
        |> required "startDate" Date.decoder
        |> required "total" Decimal.decoder
        |> required "increase" Decimal.decoder
        |> required "decrease" Decimal.decoder
        |> required "reallocated" Decimal.decoder
        |> required "categories" (Decode.list breakdownDecoder)
        |> required "transactions" (Decode.list Transaction.transactionDecoder)


granularityDecoder : Decoder Granularity
granularityDecoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case value of
                    "monthly" ->
                        Decode.succeed Monthly

                    "yearly" ->
                        Decode.succeed Yearly

                    _ ->
                        Decode.fail "Unknown analytics granularity."
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
