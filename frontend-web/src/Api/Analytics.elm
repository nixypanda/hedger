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
        , ActivityMovement(..)
        , ActivityRange(..)
        , ActivityResponse
        , AssetEvent(..)
        , AssetFigures
        , BreakdownItem
        , CashflowResponse
        , CategoryBreakdown
        , DirectionBreakdown
        , FlowEvent(..)
        , FlowFigures
        , Kpi
        , LiabilityEvent(..)
        , LiabilityFigures
        , NetWorthResponse
        , PerformancePoint
        , TimelinePoint
        )
import Data.Completeness as Completeness
import Data.Date as Date
import Data.Decimal as Decimal exposing (Decimal)
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
    Decode.field "kind" activityKindDecoder
        |> Decode.andThen
            (\kind ->
                Decode.succeed
                    (\metadata commodity range period fiscal buckets warnings ->
                        ActivityResponse metadata
                            commodity
                            kind
                            range
                            period
                            fiscal
                            buckets
                            warnings
                    )
                    |> required "metadata" Metadata.decoder
                    |> required "displayCommodity" Decode.string
                    |> required "range" activityRangeDecoder
                    |> required "period" Decode.string
                    |> required "fiscalYearStartMonth" Decode.int
                    |> required "buckets" (Decode.list (activityBucketDecoder kind))
                    |> required "warnings" (Decode.list Decode.string)
            )


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


activityBucketDecoder : ActivityKind -> Decoder ActivityBucket
activityBucketDecoder kind =
    Decode.map2 (\movement partial -> partial movement)
        (movementDecoder kind)
        (Decode.succeed
            (\key startDate total categories transactions movement ->
                AnalyticsData.ActivityBucket key startDate total categories movement transactions
            )
            |> required "key" Decode.string
            |> required "startDate" Date.decoder
            |> required "total" Decimal.decoder
            |> required "categories" (Decode.list breakdownDecoder)
            |> required "transactions" (Decode.list Transaction.transactionDecoder)
        )


movementDecoder : ActivityKind -> Decoder ActivityMovement
movementDecoder kind =
    case kind of
        AssetActivity ->
            Decode.map AssetMovements assetFiguresDecoder

        LiabilityActivity ->
            Decode.map LiabilityMovements liabilityFiguresDecoder

        IncomeActivity ->
            Decode.map IncomeMovements (flowFiguresDecoder "income")

        ExpenseActivity ->
            Decode.map ExpenseMovements (flowFiguresDecoder "expense")


assetFiguresDecoder : Decoder AssetFigures
assetFiguresDecoder =
    Decode.succeed AnalyticsData.AssetFigures
        |> required "increase" Decimal.decoder
        |> required "decrease" Decimal.decoder
        |> required "reallocated" Decimal.decoder
        |> required "directions" (Decode.list (directionBreakdownDecoder AssetActivity))
        |> required "events" (Decode.list assetEventDecoder)


liabilityFiguresDecoder : Decoder LiabilityFigures
liabilityFiguresDecoder =
    Decode.succeed AnalyticsData.LiabilityFigures
        |> required "borrowed" Decimal.decoder
        |> required "repaid" Decimal.decoder
        |> required "forgiven" Decimal.decoder
        |> required "borrowingReversed" Decimal.decoder
        |> required "netDebtMovement" Decimal.decoder
        |> required "directions" (Decode.list (directionBreakdownDecoder LiabilityActivity))
        |> required "events" (Decode.list liabilityEventDecoder)


flowFiguresDecoder : String -> Decoder FlowFigures
flowFiguresDecoder kind =
    Decode.succeed AnalyticsData.FlowFigures
        |> required "posted" Decimal.decoder
        |> required "reversed" Decimal.decoder
        |> required "events" (Decode.list (flowEventDecoder kind))


assetEventDecoder : Decoder AssetEvent
assetEventDecoder =
    Decode.field "effect" Decode.string
        |> Decode.andThen
            (\effect ->
                case effect of
                    "acquired" ->
                        simpleEvent Acquired

                    "disposed" ->
                        simpleEvent Disposed

                    "transferred" ->
                        linkedEvent Transferred

                    _ ->
                        Decode.fail ("Unexpected asset effect: " ++ effect)
            )


liabilityEventDecoder : Decoder LiabilityEvent
liabilityEventDecoder =
    Decode.field "effect" Decode.string
        |> Decode.andThen
            (\effect ->
                case effect of
                    "borrowed" ->
                        simpleEvent Borrowed

                    "repaid" ->
                        simpleEvent Repaid

                    "refinanced" ->
                        linkedEvent Refinanced

                    "forgiven" ->
                        simpleEvent Forgiven

                    "borrowing_reversed" ->
                        simpleEvent BorrowingReversed

                    _ ->
                        Decode.fail ("Unexpected liability effect: " ++ effect)
            )


flowEventDecoder : String -> Decoder FlowEvent
flowEventDecoder kind =
    Decode.field "effect" Decode.string
        |> Decode.andThen
            (\effect ->
                case effect of
                    "income_posted" ->
                        if kind == "income" then
                            simpleEvent FlowPosted

                        else
                            Decode.fail "Income effect in expense activity"

                    "income_reversed" ->
                        if kind == "income" then
                            simpleEvent FlowReversed

                        else
                            Decode.fail "Income effect in expense activity"

                    "expense_posted" ->
                        if kind == "expense" then
                            simpleEvent FlowPosted

                        else
                            Decode.fail "Expense effect in income activity"

                    "expense_reversed" ->
                        if kind == "expense" then
                            simpleEvent FlowReversed

                        else
                            Decode.fail "Expense effect in income activity"

                    _ ->
                        Decode.fail ("Unexpected flow effect: " ++ effect)
            )


simpleEvent : (String -> Decimal -> event) -> Decoder event
simpleEvent constructor =
    Decode.map2 constructor
        (Decode.field "account" Decode.string)
        (Decode.field "amount" Decimal.decoder)


linkedEvent : (String -> Decimal -> String -> Decimal -> event) -> Decoder event
linkedEvent constructor =
    Decode.map4 constructor
        (Decode.field "source" Decode.string)
        (Decode.field "sourceAmount" Decimal.decoder)
        (Decode.field "destination" Decode.string)
        (Decode.field "destinationAmount" Decimal.decoder)


directionBreakdownDecoder : ActivityKind -> Decoder DirectionBreakdown
directionBreakdownDecoder kind =
    Decode.succeed AnalyticsData.DirectionBreakdown
        |> required "direction" (activityDirectionDecoder kind)
        |> required "categories" (Decode.list breakdownDecoder)


activityDirectionDecoder : ActivityKind -> Decoder ActivityDirection
activityDirectionDecoder kind =
    Decode.string
        |> Decode.andThen
            (\value ->
                case value of
                    "increase" ->
                        Decode.succeed AnalyticsData.Increase

                    "reallocate" ->
                        if kind == AssetActivity then
                            Decode.succeed AnalyticsData.Reallocate

                        else
                            Decode.fail "Reallocation in non-asset activity"

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
