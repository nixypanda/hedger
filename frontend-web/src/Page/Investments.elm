module Page.Investments exposing
    ( ChartMode(..)
    , Model
    , Msg(..)
    , accountPrefix
    , chartMode
    , granularity
    , init
    , selectedPeriod
    , update
    , view
    )

import Api.Client as Api
import Api.Error as ApiError exposing (ApiError)
import Api.Investments as Investments
import Data.Completeness exposing (Completeness(..))
import Data.Decimal as Decimal exposing (Decimal)
import Data.Formatting as Formatting
import Data.Freshness as DataFreshness
import Data.Granularity exposing (Granularity(..))
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes as Attr
import Lib.ListExtra as ListExtra
import Lib.Number as Number
import Lib.RemoteData as RemoteData exposing (RemoteData(..))
import Ui.Button as Button
import Ui.Chart.Cartesian as Cartesian
import Ui.Chip as Chip
import Ui.Color as Color
import Ui.Common as Common
import Ui.DesignTokens as Tokens
import Ui.Env exposing (Env)
import Ui.Feedback as Feedback
import Ui.Icon as Icon exposing (Icon)
import Ui.Input
import Ui.Notice as Notice
import Ui.Page as Page
import Ui.Skeleton as Skeleton
import Ui.Surface as Surface
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import Ui.Viewport as Viewport exposing (Viewport)
import View.Analytics.Kpi as Kpi
import View.States as States



-- MODEL


type Model
    = Model ModelData


type alias ModelData =
    { response : RemoteData ApiError Investments.Response
    , nextRequestToken : Int
    , activeRequestToken : Int
    , granularity : Granularity
    , chartMode : ChartMode
    , accountPrefix : Maybe String
    , selectedPeriod : Maybe String
    }


type ChartMode
    = ValueChart
    | ReturnsChart


init : ( Model, Cmd Msg )
init =
    let
        data =
            { response = Loading Nothing
            , nextRequestToken = 1
            , activeRequestToken = 1
            , granularity = Monthly
            , chartMode = ValueChart
            , accountPrefix = Nothing
            , selectedPeriod = Nothing
            }
    in
    ( Model data, fetch data.activeRequestToken data )



-- UPDATE


type Msg
    = Refresh
    | ResponseLoaded Int (Result ApiError Investments.Response)
    | GranularitySelected Granularity
    | ChartModeSelected ChartMode
    | AccountSelected String
    | PortfolioSelected
    | PeriodSelected String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model data) =
    case msg of
        Refresh ->
            reload data

        GranularitySelected selected ->
            reload { data | granularity = selected, selectedPeriod = Nothing }

        ChartModeSelected selected ->
            ( Model { data | chartMode = selected }, Cmd.none )

        AccountSelected account ->
            loadScope { data | accountPrefix = Just account, selectedPeriod = Nothing }

        PortfolioSelected ->
            loadScope { data | accountPrefix = Nothing, selectedPeriod = Nothing }

        PeriodSelected period ->
            ( Model { data | selectedPeriod = Just period }, Cmd.none )

        ResponseLoaded token result ->
            if token /= data.activeRequestToken then
                ( Model data, Cmd.none )

            else
                case result of
                    Ok loadedResponse ->
                        ( Model
                            { data
                                | response = Success loadedResponse
                                , granularity = loadedResponse.granularity
                                , accountPrefix = loadedResponse.accountPrefix
                                , selectedPeriod =
                                    synchronizedPeriod
                                        data.selectedPeriod
                                        loadedResponse.timeline
                            }
                        , Cmd.none
                        )

                    Err error ->
                        ( Model
                            { data
                                | response =
                                    Failure error (RemoteData.value data.response)
                            }
                        , Cmd.none
                        )


reload : ModelData -> ( Model, Cmd Msg )
reload data =
    beginRequest
        (RemoteData.beginLoading data.response)
        { data
            | selectedPeriod =
                synchronizedPeriod data.selectedPeriod (timelineValue data.response)
        }


timelineValue : RemoteData error Investments.Response -> List Investments.TimelinePoint
timelineValue remote =
    remote
        |> RemoteData.value
        |> Maybe.map .timeline
        |> Maybe.withDefault []


loadScope : ModelData -> ( Model, Cmd Msg )
loadScope data =
    beginRequest (Loading Nothing) data


beginRequest : RemoteData ApiError Investments.Response -> ModelData -> ( Model, Cmd Msg )
beginRequest nextResponse data =
    let
        token =
            data.nextRequestToken + 1

        next =
            { data
                | response = nextResponse
                , nextRequestToken = token
                , activeRequestToken = token
            }
    in
    ( Model next, fetch token next )


fetch : Int -> ModelData -> Cmd Msg
fetch token data =
    Api.getInvestments
        { granularity = data.granularity
        , displayCommodity = Nothing
        , accountPrefix = data.accountPrefix
        }
        (ResponseLoaded token)


accountPrefix : Model -> Maybe String
accountPrefix (Model data) =
    data.accountPrefix


granularity : Model -> Granularity
granularity (Model data) =
    data.granularity


chartMode : Model -> ChartMode
chartMode (Model data) =
    data.chartMode


selectedPeriod : Model -> Maybe String
selectedPeriod (Model data) =
    data.selectedPeriod


synchronizedPeriod : Maybe String -> List Investments.TimelinePoint -> Maybe String
synchronizedPeriod current points =
    case current of
        Just period ->
            if List.any (.period >> (==) period) points then
                Just period

            else
                latestPeriod points

        Nothing ->
            latestPeriod points


latestPeriod : List Investments.TimelinePoint -> Maybe String
latestPeriod =
    List.reverse >> List.head >> Maybe.map .period



-- VIEW


view : Env -> Model -> Element Msg
view env (Model data) =
    let
        theme =
            env.theme

        viewport =
            env.viewport
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color theme.pageBg
        , Element.scrollbarY
        , Common.uiClass "page-scroll analytics-page"
        ]
        [ Page.pageHeaderBand theme <|
            Page.pageHeader theme
                { leading = Nothing
                , title = Typography.pageTitle theme "Investments"
                , subtitle = Just "Value, invested capital, and investment performance"
                , actions =
                    [ granularityControl theme data.granularity
                    , States.refreshStatus theme
                        { freshness = responseFreshness data.response
                        , loading = responseLoading data.response
                        }
                    , Button.refreshButton theme
                        { loading = responseLoading data.response
                        , onPress = Refresh
                        }
                    ]
                }
        , Page.pageContainer (content theme viewport data)
        ]


granularityControl : Theme -> Granularity -> Element Msg
granularityControl theme selected =
    Ui.Input.wideSegmentedControl theme
        [ { label = "M", selected = selected == Monthly, onPress = GranularitySelected Monthly }
        , { label = "Y", selected = selected == Yearly, onPress = GranularitySelected Yearly }
        ]


content : Theme -> Viewport -> ModelData -> List (Element Msg)
content theme viewport data =
    case RemoteData.value data.response of
        Nothing ->
            case data.response of
                Failure error _ ->
                    [ Feedback.errorBanner theme (ApiError.message error)
                    , Button.secondaryButton theme { label = "Retry", onPress = Refresh }
                    ]

                _ ->
                    [ investmentsSkeleton theme viewport ]

        Just response ->
            (if isEmpty response then
                emptyInvestmentsDashboard theme viewport response

             else
                investmentDashboard
                    theme
                    viewport
                    data.chartMode
                    data.selectedPeriod
                    response
            )
                :: refreshFailureNotice theme data.response


isEmpty : Investments.Response -> Bool
isEmpty response =
    response.summary.marketValue
        == Nothing
        && List.isEmpty response.children
        && List.isEmpty response.timeline


investmentDashboard :
    Theme
    -> Viewport
    -> ChartMode
    -> Maybe String
    -> Investments.Response
    -> Element Msg
investmentDashboard theme viewport selectedMode selected response =
    let
        layout =
            Viewport.contentLayout viewport

        chartWidth =
            if Viewport.isCompact layout then
                min 1200 (max 320 (Viewport.usableContentWidth viewport - 40))

            else
                min 1200 (max 320 (Viewport.usableContentWidth viewport - 376))
    in
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ]
        [ investmentSummaryGrid layout theme response.displayCommodity response.summary
        , assetClassAllocationCard
            layout
            theme
            response.displayCommodity
            response.assetClassAllocations
        , investmentMasterDetail
            layout
            theme
            response.displayCommodity
            chartWidth
            selectedMode
            selected
            response
        , investmentWarnings theme response.warnings
        ]


emptyInvestmentsDashboard : Theme -> Viewport -> Investments.Response -> Element Msg
emptyInvestmentsDashboard theme viewport response =
    let
        layout =
            Viewport.contentLayout viewport
    in
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ]
        [ investmentSummaryGrid layout theme response.displayCommodity emptySummary
        , assetClassAllocationCard
            layout
            theme
            response.displayCommodity
            response.assetClassAllocations
        , emptyInvestmentMasterDetail layout theme
        , investmentWarnings theme response.warnings
        ]


emptySummary : Investments.Summary
emptySummary =
    { marketValue = Nothing
    , purchaseCost = Nothing
    , gainLoss = Nothing
    , xirr = Nothing
    , attributedIncome = Nothing
    , realisedGain = Nothing
    , totalProfit = Nothing
    , completeness = Unavailable
    }


emptyInvestmentMasterDetail : Viewport.ContentLayout -> Theme -> Element Msg
emptyInvestmentMasterDetail layout theme =
    let
        compact =
            Viewport.isCompact layout

        navigator =
            Surface.cardWith
                (Element.width Element.fill
                    :: (if compact then
                            [ Element.height
                                (Element.minimum
                                    Tokens.investmentNavigatorCompactHeight
                                    Element.shrink
                                )
                            ]

                        else
                            [ Element.height Element.fill ]
                       )
                )
                theme
                [ Typography.mutedCaption theme "Current scope"
                , Typography.mutedParagraph theme
                    "No investment accounts are available for this scope."
                ]

        history =
            Surface.cardWith
                [ Element.height (Element.px Tokens.investmentHistoryCardHeight) ]
                theme
                [ Typography.cardTitle theme "Portfolio history"
                , Element.el
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.padding Tokens.space12
                    ]
                    (Typography.mutedParagraph theme
                        "No historical valuations are available for this scope."
                    )
                ]
    in
    if compact then
        Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ]
            [ navigator, history ]

    else
        Element.row
            [ Element.width Element.fill
            , Element.spacing Tokens.space16
            , Element.alignTop
            ]
            [ Element.el
                [ Element.width (Element.px Tokens.investmentNavigatorWidth)
                , Element.alignTop
                , Element.height Element.fill
                ]
                navigator
            , Element.el [ Element.width Element.fill, Element.alignTop ] history
            ]


assetClassAllocationCard :
    Viewport.ContentLayout
    -> Theme
    -> String
    -> List Investments.AssetClassAllocation
    -> Element Msg
assetClassAllocationCard layout theme commodity allocations =
    let
        gridHeight =
            allocationCardHeightFor layout
                - 2
                * Tokens.space20
                - 2
                * Tokens.borderHairline
                - Tokens.chartTitleHeight
                - Tokens.space12
    in
    Surface.cardWith
        [ Element.height (Element.px (allocationCardHeightFor layout)) ]
        theme
        [ Typography.cardTitle theme "Asset allocation"
        , if List.isEmpty allocations then
            Element.el
                [ Element.width Element.fill
                , Element.height (Element.px gridHeight)
                , Element.padding Tokens.space12
                ]
                (Typography.mutedParagraph theme
                    "No asset allocation data is available for this scope."
                )

          else
            Element.el
                [ Element.width Element.fill
                , Element.height (Element.px gridHeight)
                , Element.scrollbarY
                , Common.minHeightZero
                , Element.htmlAttribute (Attr.tabindex 0)
                , Element.htmlAttribute (Attr.attribute "role" "region")
                , Element.htmlAttribute (Attr.attribute "aria-label" "Asset allocation rows")
                , Element.focused
                    [ Border.shadow
                        { color = theme.primary
                        , offset = ( 0, 0 )
                        , blur = 3
                        , size = 0
                        }
                    ]
                ]
                (allocationGrid layout theme commodity allocations)
        ]


allocationCardHeightFor : Viewport.ContentLayout -> Int
allocationCardHeightFor layout =
    case layout of
        Viewport.Compact ->
            Tokens.investmentAllocationCardCompactHeight

        Viewport.Medium ->
            Tokens.investmentAllocationCardMediumHeight

        Viewport.Wide ->
            Tokens.investmentAllocationCardHeight


allocationGrid :
    Viewport.ContentLayout
    -> Theme
    -> String
    -> List Investments.AssetClassAllocation
    -> Element Msg
allocationGrid layout theme commodity allocations =
    let
        columnCount =
            case layout of
                Viewport.Compact ->
                    1

                Viewport.Medium ->
                    2

                Viewport.Wide ->
                    3

        cells =
            allocations
                |> List.map Just
                |> ListExtra.padToMultiple columnCount Nothing
    in
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space12 ]
        (ListExtra.chunksOf columnCount cells
            |> List.map (allocationGridRow theme commodity)
        )


allocationGridRow :
    Theme
    -> String
    -> List (Maybe Investments.AssetClassAllocation)
    -> Element Msg
allocationGridRow theme commodity cells =
    Element.row [ Element.width Element.fill, Element.spacing Tokens.space12, Element.alignTop ]
        (List.map (allocationGridCell theme commodity) cells)


allocationGridCell :
    Theme
    -> String
    -> Maybe Investments.AssetClassAllocation
    -> Element Msg
allocationGridCell theme commodity maybeAllocation =
    case maybeAllocation of
        Just allocation ->
            allocationGridItem theme commodity allocation

        Nothing ->
            Element.el
                (Border.color theme.transparent :: allocationCardAttributes)
                Element.none


allocationCardAttributes : List (Element.Attribute msg)
allocationCardAttributes =
    [ Element.width (Element.fillPortion Tokens.layoutPortionSingle)
    , Element.height Element.fill
    , Element.padding Tokens.space12
    , Border.width Tokens.borderHairline
    , Border.rounded Tokens.radiusControl
    ]


allocationGridItem :
    Theme
    -> String
    -> Investments.AssetClassAllocation
    -> Element Msg
allocationGridItem theme commodity allocation =
    Element.el
        (Border.color theme.divider :: allocationCardAttributes)
        (assetClassAllocationRow theme commodity allocation)


assetClassAllocationRow :
    Theme
    -> String
    -> Investments.AssetClassAllocation
    -> Element Msg
assetClassAllocationRow theme commodity allocation =
    let
        current =
            investmentDecimalFloat allocation.currentPercentage

        target =
            investmentDecimalFloat allocation.targetPercentage

        performanceColor =
            allocationPerformanceColor theme current target
    in
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
        [ Element.row [ Element.width Element.fill, Element.spacing Tokens.space12 ]
            [ Element.el
                [ Element.width (Element.px Tokens.iconSizeCompact)
                , Element.height (Element.px Tokens.iconSizeCompact)
                , Background.color theme.primarySoft
                , Border.rounded Tokens.radiusSmall
                ]
                (Element.el [ Element.centerX, Element.centerY ]
                    (Icon.view
                        { icon = investmentAssetClassIcon allocation.assetClass
                        , size = 17
                        , color = performanceColor
                        }
                    )
                )
            , Element.column [ Element.width Element.fill, Element.spacing Tokens.space2 ]
                [ Element.el
                    (Typography.body ++ [ Typography.fontWeight Typography.SemiBold ])
                    (Element.text (assetClassLabel allocation.assetClass))
                , Typography.mutedCaption theme
                    (Formatting.formatMaybeDecimalDisplay commodity allocation.marketValue)
                ]
            , Element.column [ Element.alignRight, Element.spacing Tokens.space2 ]
                [ Element.el
                    (Typography.body
                        ++ Typography.numeric
                        ++ [ Typography.fontWeight Typography.SemiBold ]
                    )
                    (Element.text (allocationPercentageLabel allocation.currentPercentage))
                , Element.row [ Element.spacing Tokens.space8 ]
                    [ Typography.mutedCaption theme
                        ("Target " ++ allocationPercentageLabel allocation.targetPercentage)
                    , allocationDeltaLabel performanceColor current target
                    ]
                ]
            ]
        , allocationComparisonBar theme performanceColor current target
        ]


allocationComparisonBar :
    Theme
    -> Element.Color
    -> Maybe Float
    -> Maybe Float
    -> Element Msg
allocationComparisonBar theme currentColor current target =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px Tokens.iconSizeTiny)
        , Background.color theme.inputFill
        , Border.rounded Tokens.radiusControl
        , Element.clip
        , Element.inFront (allocationTargetMarker theme target)
        ]
        (Element.row [ Element.width Element.fill, Element.height Element.fill ]
            (allocationBarSegments currentColor current)
        )


allocationBarSegments : Element.Color -> Maybe Float -> List (Element Msg)
allocationBarSegments color percentage =
    case percentage of
        Nothing ->
            [ Element.none ]

        Just value ->
            let
                filled =
                    value
                        |> clamp 0 100
                        |> (*) 10
                        |> round
                        |> clamp 1 999
            in
            [ Element.el
                [ Element.width (Element.fillPortion filled)
                , Element.height Element.fill
                , Background.color color
                , Border.rounded Tokens.radiusPill
                ]
                Element.none
            , Element.el [ Element.width (Element.fillPortion (1000 - filled)) ] Element.none
            ]


allocationTargetMarker : Theme -> Maybe Float -> Element Msg
allocationTargetMarker theme target =
    case target of
        Nothing ->
            Element.none

        Just value ->
            let
                position =
                    value
                        |> clamp 0 100
                        |> (*) 10
                        |> round
                        |> clamp 1 999
            in
            Element.row [ Element.width Element.fill, Element.height Element.fill ]
                [ Element.el [ Element.width (Element.fillPortion position) ] Element.none
                , Element.el
                    [ Element.width (Element.px Tokens.investmentProgressWidth)
                    , Element.height Element.fill
                    , Background.color theme.textPrimary
                    ]
                    Element.none
                , Element.el
                    [ Element.width (Element.fillPortion (1000 - position)) ]
                    Element.none
                ]


allocationDeltaLabel : Element.Color -> Maybe Float -> Maybe Float -> Element Msg
allocationDeltaLabel color current target =
    case Maybe.map2 (-) current target of
        Nothing ->
            Element.none

        Just delta ->
            let
                sign =
                    if delta > 0 then
                        "+"

                    else
                        ""
            in
            Element.el
                (Typography.caption
                    ++ [ Font.color color
                       , Typography.fontWeight Typography.SemiBold
                       ]
                )
                (Element.text (sign ++ Number.roundedString delta 2 ++ " pp"))


allocationPerformanceColor :
    Theme
    -> Maybe Float
    -> Maybe Float
    -> Element.Color
allocationPerformanceColor theme current target =
    case Maybe.map2 (\actual planned -> abs (actual - planned)) current target of
        Nothing ->
            theme.neutral

        Just deviation ->
            let
                distance =
                    clamp 0 1 (deviation / 15)
            in
            if distance <= 0.5 then
                Color.mix theme.positive theme.warningSolid (distance * 2)

            else
                Color.mix theme.warningSolid theme.negative ((distance - 0.5) * 2)


allocationPercentageLabel : Maybe Decimal -> String
allocationPercentageLabel =
    Maybe.map Formatting.formatPercentage >> Maybe.withDefault "—"


assetClassLabel : Investments.AssetClass -> String
assetClassLabel assetClass =
    case assetClass of
        Investments.Equity ->
            "Equity"

        Investments.Debt ->
            "Debt"

        Investments.Cash ->
            "Cash"

        Investments.Gold ->
            "Gold"

        Investments.Hybrid ->
            "Hybrid"

        Investments.Property ->
            "Property"

        Investments.Other ->
            "Other"


investmentMasterDetail :
    Viewport.ContentLayout
    -> Theme
    -> String
    -> Int
    -> ChartMode
    -> Maybe String
    -> Investments.Response
    -> Element Msg
investmentMasterDetail layout theme commodity chartWidth selectedMode selected response =
    let
        navigator =
            investmentNavigator theme commodity response.breadcrumbs response.children

        history =
            investmentHistoryCard
                theme
                commodity
                chartWidth
                selectedMode
                selected
                response.summary
                response.timeline
    in
    if Viewport.isCompact layout then
        Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ]
            [ navigator, history ]

    else
        Element.row [ Element.width Element.fill, Element.spacing Tokens.space16, Element.alignTop ]
            [ Element.el
                [ Element.width (Element.px Tokens.investmentNavigatorWidth)
                , Element.alignTop
                , Element.height Element.fill
                ]
                navigator
            , Element.el [ Element.width Element.fill, Element.alignTop ] history
            ]


investmentHistoryCard :
    Theme
    -> String
    -> Int
    -> ChartMode
    -> Maybe String
    -> Investments.Summary
    -> List Investments.TimelinePoint
    -> Element Msg
investmentHistoryCard theme commodity chartWidth selectedMode selected summary timeline =
    Surface.card theme
        [ Element.row [ Element.width Element.fill ]
            [ Element.column [ Element.spacing Tokens.space2 ]
                [ Typography.cardTitle theme "Portfolio history"
                , investmentCompletenessBadge theme summary.completeness
                ]
            , Element.el [ Element.alignRight ] (chartModeControl theme selectedMode)
            ]
        , investmentPerformanceChart theme commodity chartWidth selectedMode selected timeline
        , selectedInvestmentPeriod theme commodity selected timeline
        , investmentChartLegend theme selectedMode timeline
        ]


chartModeControl : Theme -> ChartMode -> Element Msg
chartModeControl theme selectedMode =
    Ui.Input.segmentedControl theme
        [ Ui.Input.segmentedOption theme
            { label = "Value"
            , selected = selectedMode == ValueChart
            , onPress = ChartModeSelected ValueChart
            }
        , Ui.Input.segmentedOption theme
            { label = "Returns"
            , selected = selectedMode == ReturnsChart
            , onPress = ChartModeSelected ReturnsChart
            }
        ]


investmentPerformanceChart :
    Theme
    -> String
    -> Int
    -> ChartMode
    -> Maybe String
    -> List Investments.TimelinePoint
    -> Element Msg
investmentPerformanceChart theme commodity availableWidth selectedMode selected points =
    case selectedMode of
        ValueChart ->
            investmentValueChart theme commodity availableWidth selected points

        ReturnsChart ->
            investmentReturnsChart theme availableWidth selected points


investmentValueChart :
    Theme
    -> String
    -> Int
    -> Maybe String
    -> List Investments.TimelinePoint
    -> Element Msg
investmentValueChart theme commodity availableWidth selected points =
    let
        hasValues =
            List.any
                (\point ->
                    point.summary.marketValue
                        /= Nothing
                        || point.summary.purchaseCost
                        /= Nothing
                )
                points

        visiblePeriods =
            investmentChartLabelPeriods points
    in
    if hasValues then
        Cartesian.view theme
            { availableWidth = availableWidth
            , height = Tokens.chartHeightInvestment
            , targetSlotWidth = 52
            , label = "Investment portfolio value and invested capital over time"
            , items = points
            , key = .period
            , xLabel =
                \point ->
                    if List.member point.period visiblePeriods then
                        Just (Formatting.shortPeriodWithYear point.period)

                    else
                        Nothing
            , describe =
                \point ->
                    "Select "
                        ++ point.period
                        ++ ": portfolio value "
                        ++ Formatting.formatMaybeDecimalDisplay commodity point.summary.marketValue
                        ++ ", invested capital "
                        ++ Formatting.formatMaybeDecimalDisplay commodity point.summary.purchaseCost
            , selected = selected
            , onSelect = PeriodSelected
            , formatTick = Formatting.formatDecimalAxis commodity
            , domain = Cartesian.PaddedExtent { ratio = 0.12, minimum = 1 }
            , series =
                [ Cartesian.AreaBetween
                    { first = .summary >> .marketValue >> investmentDecimalFloat
                    , second = .summary >> .purchaseCost >> investmentDecimalFloat
                    , positiveColor = theme.positive
                    , negativeColor = theme.negative
                    , opacity = 0.18
                    }
                , Cartesian.Line
                    { value = .summary >> .purchaseCost >> investmentDecimalFloat
                    , color = theme.textMuted
                    , width = 2.2
                    , markers = Cartesian.SelectedMarkers
                    }
                , Cartesian.Line
                    { value = .summary >> .marketValue >> investmentDecimalFloat
                    , color = theme.primary
                    , width = 3
                    , markers = Cartesian.SelectedMarkers
                    }
                ]
            }

    else
        Feedback.emptyState theme "No historical valuations are available for this scope."


investmentReturnsChart :
    Theme
    -> Int
    -> Maybe String
    -> List Investments.TimelinePoint
    -> Element Msg
investmentReturnsChart theme availableWidth selected points =
    let
        hasReturns =
            List.any (.summary >> .xirr >> (/=) Nothing) points

        visiblePeriods =
            investmentChartLabelPeriods points
    in
    if hasReturns then
        Cartesian.view theme
            { availableWidth = availableWidth
            , height = Tokens.chartHeightInvestment
            , targetSlotWidth = 52
            , label = "Investment XIRR over time"
            , items = points
            , key = .period
            , xLabel =
                \point ->
                    if List.member point.period visiblePeriods then
                        Just (Formatting.shortPeriodWithYear point.period)

                    else
                        Nothing
            , describe =
                \point ->
                    "Select "
                        ++ point.period
                        ++ ": XIRR "
                        ++ investmentRate point.summary.xirr
            , selected = selected
            , onSelect = PeriodSelected
            , formatTick = investmentAxisRate
            , domain = Cartesian.PaddedExtent { ratio = 0.12, minimum = 0.5 }
            , series =
                [ Cartesian.Line
                    { value = .summary >> .xirr >> investmentRateFloat
                    , color = theme.primary
                    , width = 3
                    , markers = Cartesian.SelectedMarkers
                    }
                ]
            }

    else
        Feedback.emptyState theme "No XIRR history is available for this scope."


investmentChartLabelPeriods : List Investments.TimelinePoint -> List String
investmentChartLabelPeriods points =
    let
        labelEvery =
            max 1 (ceiling (toFloat (List.length points) / 6))

        lastIndex =
            List.length points - 1
    in
    points
        |> List.indexedMap
            (\index point ->
                if modBy labelEvery index == 0 || index == lastIndex then
                    Just point.period

                else
                    Nothing
            )
        |> List.filterMap identity


investmentBreakdownMetric : Theme -> String -> String -> Maybe Decimal -> Element msg
investmentBreakdownMetric theme commodity label value =
    Element.column
        [ Element.spacing Tokens.space4
        , Element.width (Element.fillPortion Tokens.layoutPortionSingle)
        ]
        [ Typography.mutedCaption theme label
        , Element.el
            (Typography.body
                ++ Typography.numeric
                ++ [ Typography.fontWeight Typography.SemiBold ]
            )
            (Element.text (Formatting.formatMaybeDecimalDisplay commodity value))
        ]


investmentSummaryGrid :
    Viewport.ContentLayout
    -> Theme
    -> String
    -> Investments.Summary
    -> Element Msg
investmentSummaryGrid layout theme commodity summary =
    let
        cards =
            [ Kpi.metricCard theme
                "Portfolio value"
                (Formatting.formatMaybeDecimalDisplay commodity summary.marketValue)
                Icon.Wallet
                theme.primary
            , Kpi.metricCard theme
                "XIRR since inception"
                (investmentRate summary.xirr)
                Icon.TrendingUp
                theme.textMedium
            , Kpi.metricCard theme
                "Total profit"
                (Formatting.formatMaybeDecimalDisplay commodity summary.totalProfit)
                Icon.ChartBar
                (investmentTone summary.totalProfit theme)
            ]
    in
    if Viewport.isCompact layout then
        Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ] cards

    else
        Element.row [ Element.width Element.fill, Element.spacing Tokens.space16 ] cards


investmentNavigator :
    Theme
    -> String
    -> List Investments.Breadcrumb
    -> List Investments.Child
    -> Element Msg
investmentNavigator theme commodity breadcrumbs children =
    Surface.cardWith
        [ Element.width Element.fill
        , Element.height Element.fill
        , Region.navigation
        , Element.htmlAttribute (Attr.attribute "aria-label" "Investment account navigator")
        ]
        theme
        [ Typography.mutedCaption theme "Current scope"
        , investmentNavigatorPath theme breadcrumbs
        , Surface.hairline theme
        , Typography.mutedCaption theme "Accounts in this scope"
        , if List.isEmpty children then
            Typography.mutedParagraph theme "No accounts below this level."

          else
            Element.column [ Element.width Element.fill, Element.spacing Tokens.space4 ]
                (List.map (investmentNavigatorChild theme commodity) children)
        ]


investmentNavigatorPath : Theme -> List Investments.Breadcrumb -> Element Msg
investmentNavigatorPath theme breadcrumbs =
    let
        lastIndex =
            List.length breadcrumbs - 1
    in
    Element.wrappedRow [ Element.width Element.fill, Element.spacing Tokens.space8 ]
        (investmentNavigatorPathButton theme "Portfolio" PortfolioSelected
            :: List.concatMap
                (\( index, crumb ) ->
                    [ Icon.view
                        { icon = Icon.ChevronRight
                        , size = Tokens.iconSizeTinyGlyph
                        , color = theme.textMuted
                        }
                    , if index == lastIndex then
                        Element.el
                            (Typography.body
                                ++ [ Element.paddingXY Tokens.space8 Tokens.space8
                                   , Background.color theme.selectedBg
                                   , Border.rounded Tokens.radiusSmall
                                   , Font.color theme.textPrimary
                                   , Typography.fontWeight Typography.SemiBold
                                   ]
                            )
                            (Element.text crumb.label)

                      else
                        investmentNavigatorPathButton
                            theme
                            crumb.label
                            (AccountSelected crumb.account)
                    ]
                )
                (List.indexedMap Tuple.pair breadcrumbs)
        )


investmentNavigatorPathButton : Theme -> String -> Msg -> Element Msg
investmentNavigatorPathButton theme label onPress =
    Input.button
        (Typography.label
            ++ [ Element.htmlAttribute (Attr.title ("Open " ++ label))
               , Common.touchTargetHeight
               , Element.paddingXY Tokens.space8 Tokens.space8
               , Border.rounded Tokens.radiusSmall
               , Font.color theme.primary
               ]
            ++ Common.hoverBackground Common.HoverPrimarySoft
        )
        { onPress = Just onPress, label = Element.text label }


investmentNavigatorChild : Theme -> String -> Investments.Child -> Element Msg
investmentNavigatorChild theme commodity child =
    Input.button
        [ Element.width Element.fill
        , Element.padding Tokens.space8
        , Border.rounded Tokens.radiusSmall
        , Element.htmlAttribute (Attr.title ("Open " ++ child.label))
        ]
        { onPress = Just (AccountSelected child.account)
        , label =
            Element.row [ Element.width Element.fill, Element.spacing Tokens.space8 ]
                [ Icon.view
                    { icon = investmentAssetClassIcon child.assetClass
                    , size = Tokens.iconSizeSmall
                    , color = theme.primary
                    }
                , Element.column [ Element.width Element.fill, Element.spacing Tokens.space4 ]
                    [ Element.el
                        (Typography.body ++ [ Typography.fontWeight Typography.SemiBold ])
                        (Element.text child.label)
                    , Element.wrappedRow
                        [ Element.width Element.fill
                        , Element.spacing Tokens.space8
                        ]
                        [ investmentCompletenessBadge theme child.summary.completeness
                        , child.summary.xirr
                            |> Maybe.map
                                (\value ->
                                    Typography.mutedCaption theme
                                        (Formatting.formatRatePercentage value ++ " XIRR")
                                )
                            |> Maybe.withDefault Element.none
                        ]
                    ]
                , Element.column [ Element.alignRight, Element.spacing Tokens.space4 ]
                    [ Element.el
                        (Typography.body
                            ++ Typography.numeric
                            ++ [ Typography.fontWeight Typography.SemiBold ]
                        )
                        (Element.text
                            (Formatting.formatMaybeDecimalDisplay
                                commodity
                                child.summary.marketValue
                            )
                        )
                    , child.allocationPercentage
                        |> Maybe.map Formatting.formatPercentage
                        |> Maybe.map (Typography.mutedCaption theme)
                        |> Maybe.withDefault Element.none
                    ]
                , if child.hasChildren then
                    Icon.view
                        { icon = Icon.ChevronRight
                        , size = Tokens.iconSizeCaption
                        , color = theme.textMuted
                        }

                  else
                    Element.none
                ]
        }


investmentAssetClassIcon : Investments.AssetClass -> Icon
investmentAssetClassIcon assetClass =
    case assetClass of
        Investments.Equity ->
            Icon.Equity

        Investments.Debt ->
            Icon.Debt

        Investments.Cash ->
            Icon.Cash

        Investments.Gold ->
            Icon.Gold

        Investments.Hybrid ->
            Icon.Hybrid

        Investments.Property ->
            Icon.Property

        Investments.Other ->
            Icon.Wallet


investmentWarnings : Theme -> List Investments.Warning -> Element Msg
investmentWarnings theme warnings =
    if List.isEmpty warnings then
        Element.none

    else
        Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
            (Typography.sectionTitle theme "Data notes"
                :: List.map
                    (\warning ->
                        Feedback.errorBanner theme
                            (warning.account
                                |> Maybe.map (\account -> account ++ ": " ++ warning.message)
                                |> Maybe.withDefault warning.message
                            )
                    )
                    warnings
            )


investmentCompletenessBadge : Theme -> Completeness -> Element msg
investmentCompletenessBadge theme completeness =
    case completeness of
        Complete ->
            Element.none

        Partial ->
            Chip.statusBadge theme Chip.StatusWarning "Partial"

        Unavailable ->
            Chip.statusBadge theme Chip.StatusDanger "Unavailable"


selectedInvestmentPeriod :
    Theme
    -> String
    -> Maybe String
    -> List Investments.TimelinePoint
    -> Element msg
selectedInvestmentPeriod theme commodity selected points =
    let
        selectedPoint =
            selected
                |> Maybe.andThen
                    (\period -> points |> List.filter (.period >> (==) period) |> List.head)
    in
    case selectedPoint of
        Nothing ->
            Element.none

        Just point ->
            Element.column
                [ Element.width Element.fill
                , Element.spacing Tokens.space8
                , Element.padding Tokens.space12
                , Background.color theme.primarySoft
                , Border.rounded Tokens.radiusControl
                ]
                [ Element.row [ Element.width Element.fill ]
                    [ Element.el
                        (Typography.body ++ [ Typography.fontWeight Typography.SemiBold ])
                        (Element.text (investmentPeriodLabel point.period))
                    , Element.el [ Element.alignRight ]
                        (investmentCompletenessBadge theme point.summary.completeness)
                    ]
                , Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
                    [ Element.row [ Element.width Element.fill, Element.spacing Tokens.space16 ]
                        [ investmentBreakdownMetric
                            theme
                            commodity
                            "Invested capital"
                            point.summary.purchaseCost
                        , investmentBreakdownMetric
                            theme
                            commodity
                            "Unrealised gain"
                            point.summary.gainLoss
                        ]
                    , Element.row [ Element.width Element.fill, Element.spacing Tokens.space16 ]
                        [ investmentBreakdownMetric theme
                            commodity
                            "Attributed income"
                            point.summary.attributedIncome
                        , investmentBreakdownMetric
                            theme
                            commodity
                            "Realised gain"
                            point.summary.realisedGain
                        ]
                    ]
                ]


investmentChartLegend : Theme -> ChartMode -> List Investments.TimelinePoint -> Element msg
investmentChartLegend theme selectedMode points =
    let
        hasValues =
            case selectedMode of
                ValueChart ->
                    List.any
                        (\point ->
                            point.summary.marketValue
                                /= Nothing
                                || point.summary.purchaseCost
                                /= Nothing
                        )
                        points

                ReturnsChart ->
                    List.any (.summary >> .xirr >> (/=) Nothing) points
    in
    if not hasValues then
        Element.none

    else
        case selectedMode of
            ValueChart ->
                Element.wrappedRow [ Element.spacing Tokens.space16 ]
                    [ Element.row [ Element.spacing Tokens.space8 ]
                        [ Surface.accentDot theme.primary
                        , Typography.mutedCaption theme "Portfolio value"
                        ]
                    , Element.row [ Element.spacing Tokens.space8 ]
                        [ Surface.accentDot theme.textMuted
                        , Typography.mutedCaption theme "Invested capital"
                        ]
                    , Element.row [ Element.spacing Tokens.space8 ]
                        [ Surface.accentDot theme.positive, Typography.mutedCaption theme "Gain" ]
                    , Element.row [ Element.spacing Tokens.space8 ]
                        [ Surface.accentDot theme.negative, Typography.mutedCaption theme "Loss" ]
                    ]

            ReturnsChart ->
                Element.row [ Element.spacing Tokens.space8 ]
                    [ Surface.accentDot theme.primary, Typography.mutedCaption theme "XIRR" ]


investmentTone : Maybe Decimal -> Theme -> Element.Color
investmentTone value theme =
    case investmentDecimalFloat value of
        Just amount ->
            if amount < 0 then
                theme.negative

            else
                theme.positive

        Nothing ->
            theme.textMuted


investmentRate : Maybe Decimal -> String
investmentRate =
    Maybe.map Formatting.formatRatePercentage >> Maybe.withDefault "—"


investmentAxisRate : Float -> String
investmentAxisRate value =
    Number.roundedString value 1 ++ "%"


investmentPeriodLabel : String -> String
investmentPeriodLabel period =
    "Selected " ++ period


investmentDecimalFloat : Maybe Decimal -> Maybe Float
investmentDecimalFloat =
    Maybe.map Decimal.toFloat


investmentRateFloat : Maybe Decimal -> Maybe Float
investmentRateFloat =
    investmentDecimalFloat >> Maybe.map ((*) 100)


investmentsSkeleton : Theme -> Viewport -> Element Msg
investmentsSkeleton theme viewport =
    let
        layout =
            Viewport.contentLayout viewport

        isCompact =
            Viewport.isCompact layout

        metricCards =
            [ Skeleton.metricCard theme
            , Skeleton.metricCard theme
            , Skeleton.metricCard theme
            ]
    in
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ]
        [ if isCompact then
            Element.column
                [ Element.width Element.fill
                , Element.spacing Tokens.space16
                ]
                metricCards

          else
            Element.row [ Element.width Element.fill, Element.spacing Tokens.space16 ] metricCards
        , Skeleton.allocationCard theme layout
        , if isCompact then
            Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ]
                [ investmentNavigatorSkeleton
                    [ Element.height
                        (Element.minimum
                            Tokens.investmentNavigatorCompactHeight
                            Element.shrink
                        )
                    ]
                    theme
                , Skeleton.historyCard theme
                ]

          else
            Element.row
                [ Element.width Element.fill
                , Element.spacing Tokens.space16
                , Element.alignTop
                ]
                [ Element.el
                    [ Element.width (Element.px Tokens.investmentNavigatorWidth)
                    , Element.alignTop
                    , Element.height Element.fill
                    ]
                    (investmentNavigatorSkeleton [ Element.height Element.fill ] theme)
                , Element.el [ Element.width Element.fill, Element.alignTop ]
                    (Skeleton.historyCard theme)
                ]
        ]


investmentNavigatorSkeleton : List (Element.Attribute msg) -> Theme -> Element msg
investmentNavigatorSkeleton attributes theme =
    Surface.cardWith attributes
        theme
        [ Element.el
            [ Element.width (Element.px Tokens.periodMonthLabelWidth)
            , Element.height (Element.px Tokens.iconSizeCaption)
            , Background.color theme.inputFill
            , Border.rounded Tokens.radiusControl
            , Common.uiClass "ui-skeleton"
            ]
            Element.none
        , Element.el
            [ Element.width (Element.px Tokens.skeletonLineLong)
            , Element.height (Element.px Tokens.iconSizeTiny)
            , Background.color theme.inputFill
            , Border.rounded Tokens.radiusControl
            , Common.uiClass "ui-skeleton"
            ]
            Element.none
        , Surface.hairline theme
        , Skeleton.listRow theme
        , Surface.hairline theme
        , Skeleton.listRow theme
        ]


responseLoading : RemoteData error value -> Bool
responseLoading remote =
    case remote of
        Loading _ ->
            True

        _ ->
            False


responseFreshness : RemoteData ApiError Investments.Response -> DataFreshness.Freshness
responseFreshness remote =
    case ( remote, RemoteData.value remote ) of
        ( Failure _ (Just _), _ ) ->
            DataFreshness.Stale

        ( Failure _ Nothing, _ ) ->
            DataFreshness.Unavailable

        ( _, Just response ) ->
            response.metadata.freshness

        ( _, Nothing ) ->
            DataFreshness.Unavailable


refreshFailureNotice : Theme -> RemoteData ApiError Investments.Response -> List (Element Msg)
refreshFailureNotice theme remote =
    case remote of
        Failure error (Just _) ->
            [ Notice.view theme
                { placement = Notice.Bottom
                , tone = Notice.Danger
                , icon = Icon.AlertTriangle
                , message =
                    "Refresh failed; showing stale investments: "
                        ++ ApiError.message error
                , actions = []
                , id = Just "investments-refresh-error"
                }
            ]

        _ ->
            []
