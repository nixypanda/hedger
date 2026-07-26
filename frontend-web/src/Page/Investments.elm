module Page.Investments exposing
    ( Model
    , Msg(..)
    , accountPrefix
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
import Element exposing (Device, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attr
import RemoteData exposing (RemoteData(..))
import State.Viewport exposing (Viewport)
import Ui
import Ui.Chart.Cartesian as Cartesian
import Ui.Icon as Icon exposing (Icon)
import Ui.Responsive as Responsive
import Ui.Skeleton as Skeleton
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import View.Formatting as Formatting
import View.States as States


type Model
    = Model ModelData


type alias ModelData =
    { response : RemoteData Investments.Response
    , granularity : Granularity
    , accountPrefix : Maybe String
    , selectedPeriod : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    let
        data =
            { response = Loading Nothing
            , granularity = Monthly
            , accountPrefix = Nothing
            , selectedPeriod = Nothing
            }
    in
    ( Model data, fetch data )


type Msg
    = Refresh
    | ResponseLoaded (Result ApiError Investments.Response)
    | GranularitySelected Granularity
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

        AccountSelected account ->
            reload { data | accountPrefix = Just account, selectedPeriod = Nothing }

        PortfolioSelected ->
            reload { data | accountPrefix = Nothing, selectedPeriod = Nothing }

        PeriodSelected period ->
            ( Model { data | selectedPeriod = Just period }, Cmd.none )

        ResponseLoaded result ->
            case result of
                Ok response ->
                    ( Model
                        { data
                            | response = Success response
                            , granularity = response.granularity
                            , accountPrefix = response.accountPrefix
                            , selectedPeriod = synchronizedPeriod data.selectedPeriod response.timeline
                        }
                    , Cmd.none
                    )

                Err error ->
                    ( Model { data | response = Failure error (RemoteData.value data.response) }
                    , Cmd.none
                    )


reload : ModelData -> ( Model, Cmd Msg )
reload data =
    ( Model { data | response = RemoteData.beginLoading data.response }, fetch data )


fetch : ModelData -> Cmd Msg
fetch data =
    Api.getInvestments
        { granularity = data.granularity
        , displayCommodity = Nothing
        , accountPrefix = data.accountPrefix
        }
        ResponseLoaded


accountPrefix : Model -> Maybe String
accountPrefix (Model data) =
    data.accountPrefix


granularity : Model -> Granularity
granularity (Model data) =
    data.granularity


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
    let
        device =
            Element.classifyDevice viewport
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color theme.pageBg
        , Element.scrollbarY
        , Element.htmlAttribute (Attr.class "page-scroll analytics-page")
        ]
        [ Ui.pageHeaderBand theme device <|
            Ui.pageHeader theme
                { leading = Nothing
                , title = Typography.pageTitle theme "Investments"
                , subtitle = Just "Value, cost basis, and investment performance"
                , actions =
                    [ granularityControl theme data.granularity
                    , Ui.iconButton device theme { icon = Icon.Refresh, title = "Refresh", onPress = Refresh }
                    ]
                }
        , Ui.pageContainer device (content theme viewport device data)
        ]


granularityControl : Theme -> Granularity -> Element Msg
granularityControl theme selected =
    Ui.segmentedControl theme
        [ Ui.segmentedOption theme
            { label = "M", selected = selected == Monthly, onPress = GranularitySelected Monthly }
        , Ui.segmentedOption theme
            { label = "Y", selected = selected == Yearly, onPress = GranularitySelected Yearly }
        ]


content : Theme -> Viewport -> Device -> ModelData -> List (Element Msg)
content theme viewport device data =
    case RemoteData.value data.response of
        Nothing ->
            case data.response of
                Failure error _ ->
                    [ Ui.errorBanner theme (ApiError.message error)
                    , Ui.secondaryButton theme { label = "Retry", onPress = Refresh }
                    ]

                _ ->
                    [ investmentsSkeleton theme viewport ]

        Just response ->
            [ States.remoteNotice theme response.metadata.freshness data.response
            , investmentBreadcrumbs theme response.breadcrumbs
            , if isEmpty response then
                Ui.emptyState theme "No investment holdings were found in this account scope."

              else
                investmentDashboard theme viewport device data.selectedPeriod response
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
    -> Device
    -> Maybe String
    -> Investments.Response
    -> Element Msg
investmentDashboard theme viewport device selected response =
    let
        layout =
            Responsive.contentLayout viewport device

        chartWidth =
            min 1200 (max 320 (Responsive.usableContentWidth viewport device - 40))
    in
    Element.column [ Element.width Element.fill, Element.spacing 16 ]
        [ investmentSummaryGrid layout theme response.displayCommodity response.summary
        , investmentPerformanceBreakdown theme response.displayCommodity response.summary
        , Typography.sectionTitle theme "Value and cost"
        , Ui.card theme
            [ Element.row [ Element.width Element.fill ]
                [ Typography.subsectionTitle theme "Portfolio history"
                , investmentCompletenessBadge theme response.summary.completeness
                ]
            , investmentPerformanceChart theme
                response.displayCommodity
                chartWidth
                selected
                response.timeline
            , selectedInvestmentPeriod theme
                response.displayCommodity
                selected
                response.timeline
            , investmentChartLegend theme
            ]
        , investmentChildren theme response.displayCommodity response.children
        , investmentWarnings theme response.warnings
        ]


investmentPerformanceChart :
    Theme
    -> String
    -> Int
    -> Maybe String
    -> List Investments.TimelinePoint
    -> Element Msg
investmentPerformanceChart theme commodity availableWidth selected points =
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
            , label = "Investment market value and purchase cost over time"
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
                        ++ ": market value "
                        ++ investmentMoney commodity point.summary.marketValue
                        ++ ", purchase cost "
                        ++ investmentMoney commodity point.summary.purchaseCost
            , selected = selected
            , onSelect = PeriodSelected
            , formatTick = investmentAxisMoney commodity
            , domain = Cartesian.IncludingBaseline { baseline = 0, minimumSpan = 1 }
            , series =
                [ Cartesian.Line
                    { value = .summary >> .purchaseCost >> investmentDecimalFloat
                    , color = theme.textMuted
                    , width = 2.2
                    , markers = Cartesian.AllMarkers
                    }
                , Cartesian.Line
                    { value = .summary >> .marketValue >> investmentDecimalFloat
                    , color = theme.primary
                    , width = 3
                    , markers = Cartesian.AllMarkers
                    }
                ]
            }

    else
        Ui.emptyState theme "No historical valuations are available for this scope."


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


investmentPerformanceBreakdown : Theme -> String -> Investments.Summary -> Element Msg
investmentPerformanceBreakdown theme commodity summary =
    if summary.attributedIncome == Nothing && summary.realisedGain == Nothing then
        Element.none

    else
        Ui.card theme
            [ Typography.subsectionTitle theme "Performance breakdown"
            , Element.wrappedRow [ Element.width Element.fill, Element.spacing 24 ]
                [ investmentBreakdownMetric theme commodity "Attributed income" summary.attributedIncome
                , investmentBreakdownMetric theme commodity "Realised gain" summary.realisedGain
                , investmentBreakdownMetric theme commodity "Unrealised gain" summary.gainLoss
                ]
            ]


investmentBreakdownMetric : Theme -> String -> String -> Maybe Decimal -> Element msg
investmentBreakdownMetric theme commodity label value =
    Element.column [ Element.spacing 4, Element.width (Element.fillPortion 1) ]
        [ Typography.mutedCaption theme label
        , Element.el [ Typography.financeNumber, Font.semiBold ]
            (Element.text (investmentMoney commodity value))
        ]


investmentSummaryGrid :
    Responsive.ContentLayout
    -> Theme
    -> String
    -> Investments.Summary
    -> Element Msg
investmentSummaryGrid layout theme commodity summary =
    let
        cards =
            [ investmentMetricCard theme
                commodity
                "Market value"
                summary.marketValue
                Icon.Wallet
                theme.primary
            , investmentMetricCard theme
                commodity
                "Purchase cost"
                summary.purchaseCost
                Icon.Receipt
                theme.textMedium
            , investmentMetricCard theme
                commodity
                "Gain / loss"
                summary.gainLoss
                Icon.TrendingUp
                (investmentTone summary.gainLoss theme)
            , investmentMetricCard theme
                commodity
                "Total profit"
                summary.totalProfit
                Icon.ChartBar
                (investmentTone summary.totalProfit theme)
            ]
    in
    if Responsive.isCompact layout then
        Element.column [ Element.width Element.fill, Element.spacing 16 ] cards

    else
        Element.row [ Element.width Element.fill, Element.spacing 16 ] cards


investmentMetricCard :
    Theme
    -> String
    -> String
    -> Maybe Decimal
    -> Icon
    -> Element.Color
    -> Element Msg
investmentMetricCard theme commodity title amount icon color =
    Ui.cardWith [ Element.width (Element.fillPortion 1), Element.height (Element.px 184) ]
        theme
        [ Element.row [ Element.width Element.fill, Element.spacing 12 ]
            [ Element.el
                [ Element.width (Element.px 42)
                , Element.height (Element.px 42)
                , Background.color theme.primarySoft
                , Border.rounded 10
                ]
                (Element.el [ Element.centerX, Element.centerY ]
                    (Icon.view { icon = icon, size = 22, color = color })
                )
            ]
        , Typography.mutedBody theme title
        , Element.el [ Typography.kpiValue, Typography.financeNumber, Font.bold ]
            (Element.text (investmentMoney commodity amount))
        ]


investmentChildren : Theme -> String -> List Investments.Child -> Element Msg
investmentChildren theme commodity children =
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ Typography.sectionTitle theme "Holdings"
        , if List.isEmpty children then
            Ui.card theme [ Typography.mutedParagraph theme "No accounts below this level." ]

          else
            Ui.listCard theme
                { rows = List.map (investmentChildRow theme commodity) children
                , footer = Nothing
                }
        ]


investmentChildRow : Theme -> String -> Investments.Child -> Element Msg
investmentChildRow theme commodity child =
    Ui.listRow
        { onSelect = AccountSelected child.account
        , leading = [ Icon.view { icon = Icon.Equity, size = 22, color = theme.primary } ]
        , title = Typography.subsectionTitle theme child.label
        , subtitle =
            [ investmentCompletenessBadge theme child.summary.completeness
            , child.summary.returnPercentage
                |> Maybe.map
                    (\value ->
                        Typography.mutedCaption theme (Formatting.formatPercentage value ++ " return")
                    )
                |> Maybe.withDefault Element.none
            ]
        , trailing =
            [ Element.el [ Typography.financeNumber, Font.semiBold ]
                (Element.text (investmentMoney commodity child.summary.marketValue))
            , if child.hasChildren then
                Icon.view { icon = Icon.ChevronRight, size = 18, color = theme.textMuted }

              else
                Element.none
            ]
        }


investmentBreadcrumbs : Theme -> List Investments.Breadcrumb -> Element Msg
investmentBreadcrumbs theme breadcrumbs =
    Element.wrappedRow [ Element.width Element.fill, Element.spacing 6 ]
        (Input.button []
            { onPress = Just PortfolioSelected
            , label = Element.el [ Font.color theme.primary, Font.medium ] (Element.text "All investments")
            }
            :: List.concatMap
                (\crumb ->
                    [ Icon.view { icon = Icon.ChevronRight, size = 14, color = theme.textMuted }
                    , Input.button []
                        { onPress = Just (AccountSelected crumb.account)
                        , label =
                            Element.el [ Font.color theme.primary, Font.medium ]
                                (Element.text crumb.label)
                        }
                    ]
                )
                breadcrumbs
        )


investmentWarnings : Theme -> List Investments.Warning -> Element Msg
investmentWarnings theme warnings =
    if List.isEmpty warnings then
        Element.none

    else
        Element.column [ Element.width Element.fill, Element.spacing 8 ]
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
            Ui.statusBadge theme Ui.StatusWarning "Partial"

        Unavailable ->
            Ui.statusBadge theme Ui.StatusDanger "Unavailable"


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
                , Element.spacing 8
                , Element.padding 12
                , Background.color theme.primarySoft
                ]
                [ Element.row [ Element.width Element.fill ]
                    [ Element.el [ Font.semiBold ] (Element.text (investmentPeriodLabel point.period))
                    , Element.el [ Element.alignRight ]
                        (investmentCompletenessBadge theme point.summary.completeness)
                    ]
                , Element.wrappedRow [ Element.width Element.fill, Element.spacing 18 ]
                    [ investmentBreakdownMetric theme commodity "Market value" point.summary.marketValue
                    , investmentBreakdownMetric theme commodity "Purchase cost" point.summary.purchaseCost
                    , investmentBreakdownMetric theme commodity "Gain / loss" point.summary.gainLoss
                    , investmentBreakdownMetric theme
                        commodity
                        "Attributed income"
                        point.summary.attributedIncome
                    , investmentBreakdownMetric theme commodity "Realised gain" point.summary.realisedGain
                    , investmentBreakdownMetric theme commodity "Total profit" point.summary.totalProfit
                    ]
                ]


investmentChartLegend : Theme -> Element msg
investmentChartLegend theme =
    Element.wrappedRow [ Element.spacing 16 ]
        [ Element.row [ Element.spacing 6 ]
            [ Ui.accentDot theme.primary, Typography.mutedCaption theme "Market value" ]
        , Element.row [ Element.spacing 6 ]
            [ Ui.accentDot theme.textMuted, Typography.mutedCaption theme "Purchase cost" ]
        ]


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


investmentAxisMoney : String -> Float -> String
investmentAxisMoney commodity =
    Formatting.formatAmountDisplay (Formatting.commodityDisplaySymbol commodity)


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


investmentsSkeleton : Theme -> Viewport -> Element Msg
investmentsSkeleton theme viewport =
    let
        layout =
            Responsive.contentLayout viewport (Element.classifyDevice viewport)

        isCompact =
            Responsive.isCompact layout
    in
    Element.column [ Element.width Element.fill, Element.spacing 16 ]
        [ Element.row [ Element.width Element.fill, Element.spacing 6 ]
            [ Element.el
                [ Element.width (Element.px 60)
                , Element.height (Element.px 12)
                , Background.color theme.inputFill
                , Border.rounded 6
                , Typography.uiClass "ui-skeleton"
                ]
                Element.none
            , Element.el
                [ Element.width (Element.px 80)
                , Element.height (Element.px 12)
                , Background.color theme.inputFill
                , Border.rounded 6
                , Typography.uiClass "ui-skeleton"
                ]
                Element.none
            ]
        , if isCompact then
            Element.column [ Element.width Element.fill, Element.spacing 16 ]
                [ Skeleton.kpiCard theme
                , Skeleton.kpiCard theme
                , Skeleton.kpiCard theme
                , Skeleton.kpiCard theme
                ]

          else
            Element.row [ Element.width Element.fill, Element.spacing 16 ]
                [ Skeleton.kpiCard theme
                , Skeleton.kpiCard theme
                , Skeleton.kpiCard theme
                , Skeleton.kpiCard theme
                ]
        , Skeleton.chartCard theme
        , Typography.sectionTitle theme "Holdings"
        , Ui.card theme
            [ Skeleton.listRow theme
            , Ui.hairline theme
            , Skeleton.listRow theme
            , Ui.hairline theme
            , Skeleton.listRow theme
            ]
        ]
