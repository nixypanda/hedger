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
import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Ui
import Ui.Css as Css
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
            , breadcrumbView theme response.breadcrumbs
            , if isEmpty response then
                Ui.emptyState theme "No investment holdings were found in this account scope."

              else
                dashboard theme viewport device data.selectedPeriod response
            ]


isEmpty : Investments.Response -> Bool
isEmpty response =
    response.summary.marketValue
        == Nothing
        && List.isEmpty response.children
        && List.isEmpty response.timeline


dashboard : Theme -> Viewport -> Device -> Maybe String -> Investments.Response -> Element Msg
dashboard theme viewport device selected response =
    let
        layout =
            Responsive.contentLayout viewport device

        chartWidth =
            min 1200 (max 320 (Responsive.usableContentWidth viewport device - 40))
    in
    Element.column [ Element.width Element.fill, Element.spacing 16 ]
        [ summaryGrid layout theme response.displayCommodity response.summary response.timeline
        , performanceBreakdown theme response.displayCommodity response.summary
        , Typography.sectionTitle theme "Value and cost"
        , Ui.card theme
            [ Element.row [ Element.width Element.fill ]
                [ Typography.subsectionTitle theme "Portfolio history"
                , completenessBadge theme response.summary.completeness
                ]
            , performanceChart theme response.displayCommodity chartWidth selected response.timeline
            , selectedPeriodView theme response.displayCommodity selected response.timeline
            , chartLegend theme
            ]
        , childrenView theme device response.displayCommodity response.children
        , warningsView theme response.warnings
        ]


performanceBreakdown : Theme -> String -> Investments.Summary -> Element Msg
performanceBreakdown theme commodity summary =
    if summary.attributedIncome == Nothing && summary.realisedGain == Nothing then
        Element.none

    else
        Ui.card theme
            [ Typography.subsectionTitle theme "Performance breakdown"
            , Element.wrappedRow [ Element.width Element.fill, Element.spacing 24 ]
                [ breakdownMetric theme commodity "Attributed income" summary.attributedIncome
                , breakdownMetric theme commodity "Realised gain" summary.realisedGain
                , breakdownMetric theme commodity "Unrealised gain" summary.gainLoss
                ]
            ]


breakdownMetric : Theme -> String -> String -> Maybe Decimal -> Element msg
breakdownMetric theme commodity label value =
    Element.column [ Element.spacing 4, Element.width (Element.fillPortion 1) ]
        [ Typography.mutedCaption theme label
        , Element.el [ Typography.financeNumber, Font.semiBold ]
            (Element.text (maybeMoney commodity value))
        ]


summaryGrid :
    Responsive.ContentLayout
    -> Theme
    -> String
    -> Investments.Summary
    -> List Investments.TimelinePoint
    -> Element Msg
summaryGrid layout theme commodity summary timeline =
    let
        cards =
            [ metricCard theme
                commodity
                "Market value"
                summary.marketValue
                Icon.Wallet
                theme.primary
                timeline
            , metricCard theme
                commodity
                "Purchase cost"
                summary.purchaseCost
                Icon.Receipt
                theme.textMedium
                timeline
            , metricCard theme
                commodity
                "Gain / loss"
                summary.gainLoss
                Icon.TrendingUp
                (toneFor summary.gainLoss theme)
                timeline
            , metricCard theme
                commodity
                "Total profit"
                summary.totalProfit
                Icon.ChartBar
                (toneFor summary.totalProfit theme)
                timeline
            ]
    in
    if Responsive.isCompact layout then
        Element.column [ Element.width Element.fill, Element.spacing 16 ] cards

    else
        Element.row [ Element.width Element.fill, Element.spacing 16 ] cards


metricCard :
    Theme
    -> String
    -> String
    -> Maybe Decimal
    -> Icon
    -> Element.Color
    -> List Investments.TimelinePoint
    -> Element Msg
metricCard theme commodity title amount icon color _ =
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
            (Element.text (maybeMoney commodity amount))
        ]


childrenView : Theme -> Device -> String -> List Investments.Child -> Element Msg
childrenView theme _ commodity children =
    Element.column [ Element.width Element.fill, Element.spacing 10 ]
        [ Typography.sectionTitle theme "Holdings"
        , if List.isEmpty children then
            Ui.card theme [ Typography.mutedParagraph theme "No accounts below this level." ]

          else
            Ui.listCard theme
                { rows = List.map (childRow theme commodity) children
                , footer = Nothing
                }
        ]


childRow : Theme -> String -> Investments.Child -> Element Msg
childRow theme commodity child =
    Ui.listRow
        { onSelect = AccountSelected child.account
        , leading = [ Icon.view { icon = Icon.Equity, size = 22, color = theme.primary } ]
        , title = Typography.subsectionTitle theme child.label
        , subtitle =
            [ completenessBadge theme child.summary.completeness
            , child.summary.returnPercentage
                |> Maybe.map
                    (\value ->
                        Typography.mutedCaption theme (Formatting.formatPercentage value ++ " return")
                    )
                |> Maybe.withDefault Element.none
            ]
        , trailing =
            [ Element.el [ Typography.financeNumber, Font.semiBold ]
                (Element.text (maybeMoney commodity child.summary.marketValue))
            , if child.hasChildren then
                Icon.view { icon = Icon.ChevronRight, size = 18, color = theme.textMuted }

              else
                Element.none
            ]
        }


breadcrumbView : Theme -> List Investments.Breadcrumb -> Element Msg
breadcrumbView theme breadcrumbs =
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


warningsView : Theme -> List Investments.Warning -> Element Msg
warningsView theme warnings =
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


completenessBadge : Theme -> Completeness -> Element msg
completenessBadge theme completeness =
    case completeness of
        Complete ->
            Element.none

        Partial ->
            Ui.statusBadge theme Ui.StatusWarning "Partial"

        Unavailable ->
            Ui.statusBadge theme Ui.StatusDanger "Unavailable"


toneFor : Maybe Decimal -> Theme -> Element.Color
toneFor value theme =
    case value |> Maybe.andThen (Decimal.toString >> String.toFloat) of
        Just amount ->
            if amount < 0 then
                theme.negative

            else
                theme.positive

        Nothing ->
            theme.textMuted


maybeMoney : String -> Maybe Decimal -> String
maybeMoney commodity value =
    value
        |> Maybe.andThen (Decimal.toString >> String.toFloat)
        |> Maybe.map (Formatting.formatAmountDisplay (Formatting.commodityDisplaySymbol commodity))
        |> Maybe.withDefault "—"


performanceChart :
    Theme
    -> String
    -> Int
    -> Maybe String
    -> List Investments.TimelinePoint
    -> Element Msg
performanceChart theme commodity availableWidth selected points =
    let
        width =
            max 320 availableWidth

        height =
            260

        plotLeft =
            14

        plotRight =
            toFloat width - 14

        plotTop =
            18

        plotBottom =
            210

        values =
            List.concatMap
                (\point ->
                    List.filterMap decimalFloat
                        [ point.summary.marketValue, point.summary.purchaseCost ]
                )
                points

        low =
            min 0 (List.minimum values |> Maybe.withDefault 0)

        high =
            max 1 (List.maximum values |> Maybe.withDefault 1)

        x index =
            if List.length points <= 1 then
                (plotLeft + plotRight) / 2

            else
                plotLeft + toFloat index * (plotRight - plotLeft) / toFloat (List.length points - 1)

        y value =
            plotBottom - ((value - low) / (high - low) * (plotBottom - plotTop))

        lineFor select =
            points
                |> List.indexedMap
                    (\index point ->
                        select point.summary
                            |> decimalFloat
                            |> Maybe.map
                                (\value ->
                                    String.fromFloat (x index) ++ "," ++ String.fromFloat (y value)
                                )
                    )
                |> List.filterMap identity
                |> String.join " "

        axisLabel =
            Formatting.formatAmountDisplay (Formatting.commodityDisplaySymbol commodity) high

        labelEvery =
            max 1 (ceiling (toFloat (List.length points) / 6))

        hitWidth =
            max 24 ((plotRight - plotLeft) / toFloat (max 1 (List.length points)))

        pointMarks =
            points
                |> List.indexedMap
                    (\index point ->
                        let
                            center =
                                x index

                            selectedPoint =
                                selected == Just point.period

                            marketY =
                                point.summary.marketValue
                                    |> decimalFloat
                                    |> Maybe.map y
                                    |> Maybe.withDefault plotBottom

                            costY =
                                point.summary.purchaseCost
                                    |> decimalFloat
                                    |> Maybe.map y
                                    |> Maybe.withDefault plotBottom

                            showLabel =
                                modBy labelEvery index == 0 || index == List.length points - 1
                        in
                        Svg.g []
                            [ if selectedPoint then
                                Svg.rect
                                    [ SvgA.x (String.fromFloat (center - 12))
                                    , SvgA.y (String.fromFloat plotTop)
                                    , SvgA.width "24"
                                    , SvgA.height (String.fromFloat (plotBottom - plotTop))
                                    , SvgA.fill (Css.toCssColor theme.primarySoft)
                                    ]
                                    []

                              else
                                Svg.g [] []
                            , Svg.circle
                                [ SvgA.cx (String.fromFloat center)
                                , SvgA.cy (String.fromFloat costY)
                                , SvgA.r
                                    (if selectedPoint then
                                        "4"

                                     else
                                        "2.5"
                                    )
                                , SvgA.fill (Css.toCssColor theme.cardBg)
                                , SvgA.stroke (Css.toCssColor theme.textMuted)
                                , SvgA.strokeWidth "2"
                                ]
                                []
                            , Svg.circle
                                [ SvgA.cx (String.fromFloat center)
                                , SvgA.cy (String.fromFloat marketY)
                                , SvgA.r
                                    (if selectedPoint then
                                        "5"

                                     else
                                        "3"
                                    )
                                , SvgA.fill (Css.toCssColor theme.cardBg)
                                , SvgA.stroke (Css.toCssColor theme.primary)
                                , SvgA.strokeWidth "2.5"
                                ]
                                []
                            , if showLabel then
                                Svg.text_
                                    [ SvgA.x (String.fromFloat center)
                                    , SvgA.y "239"
                                    , SvgA.textAnchor "middle"
                                    , SvgA.fill (Css.toCssColor theme.textMuted)
                                    , SvgA.fontSize "11"
                                    ]
                                    [ Svg.text (shortPeriod point.period) ]

                              else
                                Svg.g [] []
                            , Svg.rect
                                [ SvgA.x (String.fromFloat (center - hitWidth / 2))
                                , SvgA.y (String.fromFloat plotTop)
                                , SvgA.width (String.fromFloat hitWidth)
                                , SvgA.height (String.fromFloat (plotBottom - plotTop + 34))
                                , SvgA.fill "transparent"
                                , SvgA.cursor "pointer"
                                , Attr.attribute "aria-label" ("Select " ++ point.period)
                                , SvgE.onClick (PeriodSelected point.period)
                                ]
                                []
                            ]
                    )

        chart =
            Svg.svg
                [ SvgA.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height)
                , SvgA.width "100%"
                , SvgA.height (String.fromInt height)
                , Attr.attribute "role" "img"
                , Attr.attribute "aria-label" "Investment market value and purchase cost over time"
                ]
                ([ Svg.line
                    [ SvgA.x1 (String.fromFloat plotLeft)
                    , SvgA.y1 (String.fromFloat plotBottom)
                    , SvgA.x2 (String.fromFloat plotRight)
                    , SvgA.y2 (String.fromFloat plotBottom)
                    , SvgA.stroke (Css.toCssColor theme.divider)
                    ]
                    []
                 , Svg.text_
                    [ SvgA.x (String.fromFloat plotLeft)
                    , SvgA.y "13"
                    , SvgA.fill (Css.toCssColor theme.textMuted)
                    , SvgA.fontSize "11"
                    ]
                    [ Svg.text axisLabel ]
                 , Svg.polyline
                    [ SvgA.points (lineFor .purchaseCost)
                    , SvgA.fill "none"
                    , SvgA.stroke (Css.toCssColor theme.textMuted)
                    , SvgA.strokeWidth "2.2"
                    ]
                    []
                 , Svg.polyline
                    [ SvgA.points (lineFor .marketValue)
                    , SvgA.fill "none"
                    , SvgA.stroke (Css.toCssColor theme.primary)
                    , SvgA.strokeWidth "3"
                    ]
                    []
                 ]
                    ++ pointMarks
                )
    in
    if List.isEmpty values then
        Ui.emptyState theme "No historical valuations are available for this scope."

    else
        Element.el [ Element.width Element.fill, Element.height (Element.px height) ] (Element.html chart)


selectedPeriodView :
    Theme
    -> String
    -> Maybe String
    -> List Investments.TimelinePoint
    -> Element msg
selectedPeriodView theme commodity selected points =
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
                    [ Element.el [ Font.semiBold ] (Element.text (periodLabel point.period))
                    , Element.el [ Element.alignRight ]
                        (completenessBadge theme point.summary.completeness)
                    ]
                , Element.wrappedRow [ Element.width Element.fill, Element.spacing 18 ]
                    [ breakdownMetric theme commodity "Market value" point.summary.marketValue
                    , breakdownMetric theme commodity "Purchase cost" point.summary.purchaseCost
                    , breakdownMetric theme commodity "Gain / loss" point.summary.gainLoss
                    , breakdownMetric theme commodity "Attributed income" point.summary.attributedIncome
                    , breakdownMetric theme commodity "Realised gain" point.summary.realisedGain
                    , breakdownMetric theme commodity "Total profit" point.summary.totalProfit
                    ]
                ]


shortPeriod : String -> String
shortPeriod period =
    case String.split "-" period of
        [ year, month ] ->
            month ++ "/" ++ String.right 2 year

        _ ->
            period


periodLabel : String -> String
periodLabel period =
    "Selected " ++ period


decimalFloat : Maybe Decimal -> Maybe Float
decimalFloat =
    Maybe.andThen (Decimal.toString >> String.toFloat)


chartLegend : Theme -> Element msg
chartLegend theme =
    Element.wrappedRow [ Element.spacing 16 ]
        [ Element.row [ Element.spacing 6 ]
            [ Ui.accentDot theme.primary, Typography.mutedCaption theme "Market value" ]
        , Element.row [ Element.spacing 6 ]
            [ Ui.accentDot theme.textMuted, Typography.mutedCaption theme "Purchase cost" ]
        ]


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
