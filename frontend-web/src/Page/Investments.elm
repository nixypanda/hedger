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
import Data.Granularity exposing (Granularity(..))
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes as Attr
import RemoteData exposing (RemoteData(..))
import Ui
import Ui.Button as Button
import Ui.Chart.Cartesian as Cartesian
import Ui.Chip as Chip
import Ui.Color as Color
import Ui.Common as Common
import Ui.DesignTokens as Tokens
import Ui.Icon as Icon exposing (Icon)
import Ui.Input
import Ui.Skeleton as Skeleton
import Ui.Surface as Surface
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import Ui.Viewport as Viewport exposing (Viewport)
import View.Formatting as Formatting
import View.States as States


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
    beginRequest (RemoteData.beginLoading data.response) data


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


view : Theme -> Viewport -> Model -> Element Msg
view theme viewport (Model data) =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color theme.pageBg
        , Element.scrollbarY
        , Common.uiClass "page-scroll analytics-page"
        ]
        [ Ui.pageHeaderBand theme <|
            Ui.pageHeader theme
                { leading = Nothing
                , title = Typography.pageTitle theme "Investments"
                , subtitle = Just "Value, invested capital, and investment performance"
                , actions =
                    [ granularityControl theme data.granularity
                    , Button.iconButton theme
                        { icon = Icon.Refresh, title = "Refresh", onPress = Refresh }
                    ]
                }
        , Ui.pageContainer (content theme viewport data)
        ]


granularityControl : Theme -> Granularity -> Element Msg
granularityControl theme selected =
    Ui.Input.segmentedControl theme
        [ Ui.Input.segmentedOption theme
            { label = "M", selected = selected == Monthly, onPress = GranularitySelected Monthly }
        , Ui.Input.segmentedOption theme
            { label = "Y", selected = selected == Yearly, onPress = GranularitySelected Yearly }
        ]


content : Theme -> Viewport -> ModelData -> List (Element Msg)
content theme viewport data =
    case RemoteData.value data.response of
        Nothing ->
            case data.response of
                Failure error _ ->
                    [ Ui.errorBanner theme (ApiError.message error)
                    , Button.secondaryButton theme { label = "Retry", onPress = Refresh }
                    ]

                _ ->
                    [ investmentsSkeleton theme viewport ]

        Just response ->
            [ States.remoteNotice ApiError.message theme response.metadata.freshness data.response
            , if isEmpty response then
                Ui.emptyState theme "No investment holdings were found in this account scope."

              else
                investmentDashboard theme viewport data.chartMode data.selectedPeriod response
            ]


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


assetClassAllocationCard :
    Viewport.ContentLayout
    -> Theme
    -> String
    -> List Investments.AssetClassAllocation
    -> Element Msg
assetClassAllocationCard layout theme commodity allocations =
    if List.isEmpty allocations then
        Element.none

    else
        Surface.card theme
            [ Typography.subsectionTitle theme "Asset allocation"
            , allocationGrid layout theme commodity allocations
            ]


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
                |> padToMultiple columnCount Nothing
    in
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space12 ]
        (chunksOf columnCount cells
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
    , Border.rounded Tokens.radiusMedium
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


chunksOf : Int -> List value -> List (List value)
chunksOf size values =
    if List.isEmpty values then
        []

    else
        List.take size values :: chunksOf size (List.drop size values)


padToMultiple : Int -> value -> List value -> List value
padToMultiple size padding values =
    values
        ++ List.repeat
            (modBy size (size - modBy size (List.length values)))
            padding


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
                , Border.rounded Tokens.radiusIcon
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
                [ Element.el [ Font.semiBold ]
                    (Element.text (assetClassLabel allocation.assetClass))
                , Typography.mutedCaption theme (investmentMoney commodity allocation.marketValue)
                ]
            , Element.column [ Element.alignRight, Element.spacing Tokens.space2 ]
                [ Element.el [ Typography.financeNumber, Font.semiBold ]
                    (Element.text (allocationPercentageLabel allocation.currentPercentage))
                , Element.row [ Element.spacing Tokens.space6 ]
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
                , Border.rounded Tokens.radiusTight
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
            Element.el [ Font.color color, Typography.caption, Font.semiBold ]
                (Element.text (sign ++ Formatting.roundedString delta 2 ++ " pp"))


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
                [ Typography.subsectionTitle theme "Portfolio history"
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
            , height = 260
            , targetSlotWidth = 52
            , label = "Investment portfolio value and invested capital over time"
            , items = points
            , key = .period
            , xLabel =
                \point ->
                    if List.member point.period visiblePeriods then
                        Just (shortInvestmentPeriod point.period)

                    else
                        Nothing
            , describe =
                \point ->
                    "Select "
                        ++ point.period
                        ++ ": portfolio value "
                        ++ investmentMoney commodity point.summary.marketValue
                        ++ ", invested capital "
                        ++ investmentMoney commodity point.summary.purchaseCost
            , selected = selected
            , onSelect = PeriodSelected
            , formatTick = investmentAxisMoney commodity
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
        Ui.emptyState theme "No historical valuations are available for this scope."


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
            , height = 260
            , targetSlotWidth = 52
            , label = "Investment XIRR over time"
            , items = points
            , key = .period
            , xLabel =
                \point ->
                    if List.member point.period visiblePeriods then
                        Just (shortInvestmentPeriod point.period)

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
        Ui.emptyState theme "No XIRR history is available for this scope."


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
        , Element.el [ Typography.financeNumber, Font.semiBold ]
            (Element.text (investmentMoney commodity value))
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
            [ investmentMetricCard theme
                "Portfolio value"
                (investmentMoney commodity summary.marketValue)
                Icon.Wallet
                theme.primary
            , investmentMetricCard theme
                "XIRR since inception"
                (investmentRate summary.xirr)
                Icon.TrendingUp
                theme.textMedium
            , investmentMetricCard theme
                "Total profit"
                (investmentMoney commodity summary.totalProfit)
                Icon.ChartBar
                (investmentTone summary.totalProfit theme)
            ]
    in
    if Viewport.isCompact layout then
        Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ] cards

    else
        Element.row [ Element.width Element.fill, Element.spacing Tokens.space16 ] cards


investmentMetricCard :
    Theme
    -> String
    -> String
    -> Icon
    -> Element.Color
    -> Element Msg
investmentMetricCard theme title value icon color =
    Surface.cardWith [ Element.width (Element.fillPortion Tokens.layoutPortionSingle) ]
        theme
        [ Element.row
            [ Element.width Element.fill
            , Element.spacing Tokens.space12
            , Element.alignTop
            ]
            [ Element.el
                [ Element.width (Element.px Tokens.compactControlVisualHeight)
                , Element.height (Element.px Tokens.compactControlVisualHeight)
                , Background.color theme.primarySoft
                , Border.rounded Tokens.radiusMedium
                ]
                (Element.el [ Element.centerX, Element.centerY ]
                    (Icon.view { icon = icon, size = Tokens.iconSizeNavigation, color = color })
                )
            , Element.column [ Element.width Element.fill, Element.spacing Tokens.space4 ]
                [ Typography.mutedCaption theme title
                , Element.el [ Typography.kpiValue, Typography.financeNumber, Font.bold ]
                    (Element.text value)
                ]
            ]
        ]


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
    Element.wrappedRow [ Element.width Element.fill, Element.spacing Tokens.space6 ]
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
                            [ Element.paddingXY Tokens.space10 Tokens.space8
                            , Background.color theme.selectedBg
                            , Border.rounded Tokens.radiusSmall
                            , Font.color theme.textPrimary
                            , Font.semiBold
                            ]
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
        ([ Element.htmlAttribute (Attr.title ("Open " ++ label))
         , Common.touchTargetHeight
         , Element.paddingXY Tokens.space10 Tokens.space8
         , Border.rounded Tokens.radiusSmall
         , Font.color theme.primary
         , Font.medium
         ]
            ++ Common.hoverBackground theme.primarySoft
        )
        { onPress = Just onPress, label = Element.text label }


investmentNavigatorChild : Theme -> String -> Investments.Child -> Element Msg
investmentNavigatorChild theme commodity child =
    Input.button
        [ Element.width Element.fill
        , Element.padding Tokens.space10
        , Border.rounded Tokens.radiusSmall
        , Element.htmlAttribute (Attr.title ("Open " ++ child.label))
        ]
        { onPress = Just (AccountSelected child.account)
        , label =
            Element.row [ Element.width Element.fill, Element.spacing Tokens.space10 ]
                [ Icon.view
                    { icon = investmentAssetClassIcon child.assetClass
                    , size = Tokens.iconSizeSmall
                    , color = theme.primary
                    }
                , Element.column [ Element.width Element.fill, Element.spacing Tokens.space3 ]
                    [ Element.el [ Font.semiBold ] (Element.text child.label)
                    , Element.wrappedRow
                        [ Element.width Element.fill
                        , Element.spacing Tokens.space6
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
                , Element.column [ Element.alignRight, Element.spacing Tokens.space3 ]
                    [ Element.el [ Typography.financeNumber, Font.semiBold ]
                        (Element.text (investmentMoney commodity child.summary.marketValue))
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
                        Ui.errorBanner theme
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
                ]
                [ Element.row [ Element.width Element.fill ]
                    [ Element.el [ Font.semiBold ]
                        (Element.text (investmentPeriodLabel point.period))
                    , Element.el [ Element.alignRight ]
                        (investmentCompletenessBadge theme point.summary.completeness)
                    ]
                , Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
                    [ Element.row [ Element.width Element.fill, Element.spacing Tokens.space18 ]
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
                    , Element.row [ Element.width Element.fill, Element.spacing Tokens.space18 ]
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
                    [ Element.row [ Element.spacing Tokens.space6 ]
                        [ Surface.accentDot theme.primary
                        , Typography.mutedCaption theme "Portfolio value"
                        ]
                    , Element.row [ Element.spacing Tokens.space6 ]
                        [ Surface.accentDot theme.textMuted
                        , Typography.mutedCaption theme "Invested capital"
                        ]
                    , Element.row [ Element.spacing Tokens.space6 ]
                        [ Surface.accentDot theme.positive, Typography.mutedCaption theme "Gain" ]
                    , Element.row [ Element.spacing Tokens.space6 ]
                        [ Surface.accentDot theme.negative, Typography.mutedCaption theme "Loss" ]
                    ]

            ReturnsChart ->
                Element.row [ Element.spacing Tokens.space6 ]
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


investmentMoney : String -> Maybe Decimal -> String
investmentMoney commodity value =
    value
        |> investmentDecimalFloat
        |> Maybe.map (Formatting.formatAmountDisplay (Formatting.commodityDisplaySymbol commodity))
        |> Maybe.withDefault "—"


investmentRate : Maybe Decimal -> String
investmentRate =
    Maybe.map Formatting.formatRatePercentage >> Maybe.withDefault "—"


investmentAxisMoney : String -> Float -> String
investmentAxisMoney commodity =
    Formatting.formatAmountDisplay (Formatting.commodityDisplaySymbol commodity)


investmentAxisRate : Float -> String
investmentAxisRate value =
    Formatting.roundedString value 1 ++ "%"


shortInvestmentPeriod : String -> String
shortInvestmentPeriod period =
    case String.split "-" period of
        [ year, month ] ->
            month ++ "/" ++ String.right 2 year

        _ ->
            period


investmentPeriodLabel : String -> String
investmentPeriodLabel period =
    "Selected " ++ period


investmentDecimalFloat : Maybe Decimal -> Maybe Float
investmentDecimalFloat =
    Maybe.andThen (Decimal.toString >> String.toFloat)


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
    in
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ]
        [ Element.row [ Element.width Element.fill, Element.spacing Tokens.space6 ]
            [ Element.el
                [ Element.width (Element.px Tokens.skeletonLineTiny)
                , Element.height (Element.px Tokens.iconSizeTiny)
                , Background.color theme.inputFill
                , Border.rounded Tokens.radiusControl
                , Common.uiClass "ui-skeleton"
                ]
                Element.none
            , Element.el
                [ Element.width (Element.px Tokens.skeletonLineShort)
                , Element.height (Element.px Tokens.iconSizeTiny)
                , Background.color theme.inputFill
                , Border.rounded Tokens.radiusControl
                , Common.uiClass "ui-skeleton"
                ]
                Element.none
            ]
        , if isCompact then
            Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ]
                [ Skeleton.kpiCard theme
                , Skeleton.kpiCard theme
                , Skeleton.kpiCard theme
                ]

          else
            Element.row [ Element.width Element.fill, Element.spacing Tokens.space16 ]
                [ Skeleton.kpiCard theme
                , Skeleton.kpiCard theme
                , Skeleton.kpiCard theme
                ]
        , if isCompact then
            Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ]
                [ investmentNavigatorSkeleton theme
                , Skeleton.chartCard theme
                ]

          else
            Element.row
                [ Element.width Element.fill
                , Element.spacing Tokens.space16
                , Element.alignTop
                ]
                [ Element.el
                    [ Element.width (Element.px Tokens.investmentNavigatorWidth) ]
                    (investmentNavigatorSkeleton theme)
                , Element.el [ Element.width Element.fill ] (Skeleton.chartCard theme)
                ]
        ]


investmentNavigatorSkeleton : Theme -> Element msg
investmentNavigatorSkeleton theme =
    Surface.card theme
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
