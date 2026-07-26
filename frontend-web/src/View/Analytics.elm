module View.Analytics exposing
    ( ActivityTone(..)
    , LegendState(..)
    , Palette(..)
    , activityMonth
    , activityTransactions
    , activityYear
    , categoryCard
    , depthControl
    , kpiCard
    , performanceChart
    , timelineChart
    , warningList
    )

import Api.Analytics
    exposing
        ( ActivityBucket
        , ActivityResponse
        , BreakdownItem
        , CategoryBreakdown
        , PerformancePoint
        )
import Data.ActivityCalendar as ActivityCalendar
import Data.Analytics exposing (Kpi, TimelinePoint)
import Data.Calendar as Calendar
import Data.Date as Date
import Data.Decimal as Decimal exposing (Decimal)
import Data.Granularity exposing (Granularity(..))
import Data.Transaction exposing (Transaction)
import Element exposing (Device, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes as Attr
import List
import Path
import Shape
import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Ui
import Ui.Css as Css
import Ui.Icon as Icon exposing (Icon)
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import View.AccountPresentation as AccountPresentation
import View.Formatting as Formatting


type Palette
    = PositivePalette
    | NegativePalette
    | AssetPalette
    | LiabilityPalette


type ActivityTone
    = IncomeTone
    | ExpenseTone
    | AssetTone
    | LiabilityTone


type LegendState
    = LegendCollapsed
    | LegendExpanded


kpiCard :
    Theme
    ->
        { title : String
        , commodity : String
        , kpi : Kpi
        , color : Element.Color
        , icon : Icon
        , decreaseIsPositive : Bool
        }
    -> Element msg
kpiCard theme config =
    Ui.cardWith
        [ Element.width (Element.fillPortion 1)
        , Element.height (Element.px 184)
        ]
        theme
        [ Element.row [ Element.width Element.fill, Element.spacing 12 ]
            [ Element.el
                [ Element.width (Element.px 42)
                , Element.height (Element.px 42)
                , Background.color theme.primarySoft
                , Border.rounded 10
                ]
                (Element.el [ Element.centerX, Element.centerY ]
                    (Icon.view { icon = config.icon, size = 22, color = config.color })
                )
            , sparkline config.color (List.map decimalFloat config.kpi.trend)
            ]
        , Typography.mutedBody theme config.title
        , Element.el
            [ Typography.kpiValue
            , Typography.financeNumber
            , Font.bold
            ]
            (Element.text (money config.commodity config.kpi.value))
        , comparison theme config.commodity config.decreaseIsPositive config.kpi
        ]


comparison : Theme -> String -> Bool -> Kpi -> Element msg
comparison theme commodity decreaseIsPositive kpi =
    case kpi.absoluteChange of
        Nothing ->
            Typography.mutedCaption theme "No previous period"

        Just change ->
            let
                amount =
                    decimalFloat change

                color =
                    if (amount >= 0) /= decreaseIsPositive then
                        theme.positive

                    else
                        theme.negative

                percentage =
                    kpi.percentageChange
                        |> Maybe.map
                            (\value ->
                                " · " ++ Formatting.roundedString (decimalFloat value) 1 ++ "%"
                            )
                        |> Maybe.withDefault ""
            in
            Element.el
                [ Font.color color
                , Font.semiBold
                , Typography.caption
                , Typography.financeNumber
                ]
                (Element.text ("vs previous  " ++ money commodity change ++ percentage))


sparkline : Element.Color -> List Float -> Element msg
sparkline color values =
    let
        width =
            126

        height =
            48

        low =
            List.minimum values |> Maybe.withDefault 0

        high =
            List.maximum values |> Maybe.withDefault 0

        spread =
            max 0.000001 (high - low)

        count =
            List.length values

        point index value =
            let
                x =
                    if count <= 1 then
                        toFloat width / 2

                    else
                        3 + (toFloat index / toFloat (count - 1) * toFloat (width - 6))

                y =
                    3 + ((high - value) / spread * toFloat (height - 6))
            in
            String.fromFloat x ++ "," ++ String.fromFloat y
    in
    if List.isEmpty values then
        Element.none

    else
        Element.el
            [ Element.width (Element.px width)
            , Element.height (Element.px height)
            , Element.alignRight
            ]
            (Element.html
                (Svg.svg
                    [ SvgA.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height)
                    , SvgA.width "100%"
                    , SvgA.height (String.fromInt height)
                    , Attr.attribute "role" "img"
                    , Attr.attribute "aria-label" "Recent trend"
                    ]
                    [ Svg.polyline
                        [ SvgA.points (String.join " " (List.indexedMap point values))
                        , SvgA.fill "none"
                        , SvgA.stroke (Css.toCssColor color)
                        , SvgA.strokeWidth "2"
                        , SvgA.strokeLinecap "round"
                        , SvgA.strokeLinejoin "round"
                        ]
                        []
                    ]
                )
            )


timelineChart :
    Theme
    ->
        { commodity : String
        , points : List TimelinePoint
        , selected : Maybe String
        , onSelect : String -> msg
        , availableWidth : Int
        , positiveLabel : String
        , negativeLabel : String
        , netLabel : String
        }
    -> Element msg
timelineChart theme config =
    let
        layout =
            chartLayout config.availableWidth (List.length config.points) 300 40

        values =
            List.concatMap
                (\point ->
                    [ abs (decimalFloat point.positive)
                    , abs (decimalFloat point.negative)
                    , abs (decimalFloat point.net)
                    ]
                )
                config.points

        high =
            max 1 (List.maximum values |> Maybe.withDefault 1)

        y value =
            layout.plotTop + ((high - value) / (2 * high) * layout.plotHeight)

        ticks =
            [ -high, -high / 2, 0, high / 2, high ]

        x index =
            (toFloat index + 0.5) * layout.slotWidth

        selectedPoint =
            config.selected
                |> Maybe.andThen
                    (\period -> List.filter (.period >> (==) period) config.points |> List.head)

        datum index point =
            let
                center =
                    x index

                positive =
                    decimalFloat point.positive

                negative =
                    -(abs (decimalFloat point.negative))

                selected =
                    config.selected == Just point.period
            in
            Svg.g []
                [ activeBand theme layout selected center
                , roundedBar theme.positive layout.barWidth center (y 0) (y positive) positive
                , roundedBar theme.negative layout.barWidth center (y 0) (y negative) negative
                , xAxisLabel theme layout center (shortPeriod point.period)
                , if selected then
                    Svg.circle
                        [ SvgA.cx (float center)
                        , SvgA.cy (float (y (decimalFloat point.net)))
                        , SvgA.r "4.5"
                        , SvgA.fill (Css.toCssColor theme.cardBg)
                        , SvgA.stroke (Css.toCssColor theme.primary)
                        , SvgA.strokeWidth "3"
                        ]
                        []

                  else
                    Svg.g [] []
                , chartHitTarget layout
                    center
                    point.period
                    (point.period
                        ++ ": "
                        ++ String.toLower config.positiveLabel
                        ++ " "
                        ++ money config.commodity point.positive
                        ++ ", "
                        ++ String.toLower config.negativeLabel
                        ++ " "
                        ++ money config.commodity point.negative
                    )
                    (config.onSelect point.period)
                ]

        netPoints =
            config.points
                |> List.indexedMap
                    (\index point -> float (x index) ++ "," ++ float (y (decimalFloat point.net)))
                |> String.join " "

        plot =
            Svg.svg
                (plotSvgAttributes layout "Assets and liabilities timeline")
                (gridLines theme layout y ticks
                    ++ List.indexedMap datum config.points
                    ++ [ Svg.polyline
                            [ SvgA.points netPoints
                            , SvgA.fill "none"
                            , SvgA.stroke (Css.toCssColor theme.primary)
                            , SvgA.strokeWidth "2.6"
                            , SvgA.strokeLinecap "round"
                            , SvgA.strokeLinejoin "round"
                            , SvgA.pointerEvents "none"
                            ]
                            []
                       ]
                )
    in
    Element.column [ Element.width Element.fill, Element.spacing 12 ]
        [ fixedAxisChart theme config.commodity layout ticks y plot
        , selectedPoint
            |> Maybe.map
                (\point ->
                    Ui.wrappingStatusBadge theme
                        Ui.StatusPrimary
                        ("Selected "
                            ++ periodName point.period
                            ++ ": "
                            ++ String.toLower config.positiveLabel
                            ++ " "
                            ++ money config.commodity point.positive
                            ++ ", "
                            ++ String.toLower config.negativeLabel
                            ++ " "
                            ++ money config.commodity point.negative
                            ++ ", "
                            ++ String.toLower config.netLabel
                            ++ " "
                            ++ money config.commodity point.net
                        )
                )
            |> Maybe.withDefault Element.none
        ]


performanceChart :
    Theme
    ->
        { commodity : String
        , points : List PerformancePoint
        , selected : Maybe String
        , onSelect : String -> msg
        , availableWidth : Int
        }
    -> Element msg
performanceChart theme config =
    let
        layout =
            chartLayout config.availableWidth (List.length config.points) 250 52

        values =
            List.concatMap
                (\point -> [ decimalFloat point.netInvestment, decimalFloat point.netWorth ])
                config.points

        minimum =
            List.minimum values |> Maybe.withDefault 0

        maximum =
            List.maximum values |> Maybe.withDefault 1

        padding =
            max 1 (maximum - minimum) * 0.12

        low =
            minimum - padding

        high =
            maximum + padding

        y value =
            layout.plotTop + ((high - value) / (high - low) * layout.plotHeight)

        ticks =
            List.range 0 4 |> List.map (\index -> low + toFloat index * (high - low) / 4)

        x index =
            (toFloat index + 0.5) * layout.slotWidth

        pointsFor select =
            config.points
                |> List.indexedMap (\index point -> float (x index) ++ "," ++ float (y (select point)))
                |> String.join " "

        selectedPoint =
            config.selected
                |> Maybe.andThen
                    (\period -> List.filter (.period >> (==) period) config.points |> List.head)

        selectedBand index point =
            activeBand theme layout (config.selected == Just point.period) (x index)

        datum index point =
            let
                center =
                    x index

                selected =
                    config.selected == Just point.period
            in
            Svg.g []
                [ xAxisLabel theme layout center (shortPeriod point.period)
                , if selected then
                    Svg.g []
                        [ performanceMarker theme theme.primary center (y (decimalFloat point.netWorth))
                        , performanceMarker theme theme.positive center (y (decimalFloat point.netInvestment))
                        ]

                  else
                    Svg.g [] []
                , chartHitTarget layout
                    center
                    point.period
                    (point.period
                        ++ ": net investment "
                        ++ money config.commodity point.netInvestment
                        ++ ", net worth "
                        ++ money config.commodity point.netWorth
                    )
                    (config.onSelect point.period)
                ]

        plot =
            Svg.svg
                (plotSvgAttributes layout "Net investment and net worth timeline")
                (gridLines theme layout y ticks
                    ++ List.indexedMap selectedBand config.points
                    ++ (config.points
                            |> adjacentPairs
                            |> List.indexedMap
                                (\index ( first, second ) ->
                                    performanceAreaSegments theme x y index first second
                                )
                            |> List.concat
                       )
                    ++ [ Svg.polyline
                            [ SvgA.points (pointsFor (.netWorth >> decimalFloat))
                            , SvgA.fill "none"
                            , SvgA.stroke (Css.toCssColor theme.primary)
                            , SvgA.strokeWidth "2.6"
                            , SvgA.strokeLinecap "round"
                            , SvgA.strokeLinejoin "round"
                            ]
                            []
                       , Svg.polyline
                            [ SvgA.points (pointsFor (.netInvestment >> decimalFloat))
                            , SvgA.fill "none"
                            , SvgA.stroke (Css.toCssColor theme.positive)
                            , SvgA.strokeWidth "2.4"
                            , SvgA.strokeLinecap "round"
                            , SvgA.strokeLinejoin "round"
                            ]
                            []
                       ]
                    ++ List.indexedMap datum config.points
                )
    in
    Element.column [ Element.width Element.fill, Element.spacing 12 ]
        [ fixedAxisChart theme config.commodity layout ticks y plot
        , selectedPoint
            |> Maybe.map
                (\point ->
                    let
                        gainLoss =
                            decimalFloat point.gainLoss

                        label =
                            if gainLoss >= 0 then
                                "gain "

                            else
                                "loss "
                    in
                    Ui.wrappingStatusBadge theme
                        (if gainLoss >= 0 then
                            Ui.StatusSuccess

                         else
                            Ui.StatusDanger
                        )
                        ("Selected "
                            ++ periodName point.period
                            ++ ": net investment "
                            ++ money config.commodity point.netInvestment
                            ++ ", net worth "
                            ++ money config.commodity point.netWorth
                            ++ ", "
                            ++ label
                            ++ moneyFloat config.commodity (abs gainLoss)
                        )
                )
            |> Maybe.withDefault Element.none
        ]


performanceAreaSegments :
    Theme
    -> (Int -> Float)
    -> (Float -> Float)
    -> Int
    -> PerformancePoint
    -> PerformancePoint
    -> List (Svg.Svg msg)
performanceAreaSegments theme x y index first second =
    let
        firstWorth =
            decimalFloat first.netWorth

        firstInvestment =
            decimalFloat first.netInvestment

        secondWorth =
            decimalFloat second.netWorth

        secondInvestment =
            decimalFloat second.netInvestment

        firstDifference =
            firstWorth - firstInvestment

        secondDifference =
            secondWorth - secondInvestment

        firstX =
            x index

        secondX =
            x (index + 1)

        polygon color points =
            Svg.polygon
                [ SvgA.points
                    (points
                        |> List.map (\( pointX, value ) -> float pointX ++ "," ++ float (y value))
                        |> String.join " "
                    )
                , SvgA.fill (Css.toCssColor color)
                , SvgA.opacity "0.18"
                ]
                []

        colorFor difference =
            if difference >= 0 then
                theme.positive

            else
                theme.negative
    in
    if firstDifference * secondDifference >= 0 then
        [ polygon (colorFor firstDifference)
            [ ( firstX, firstWorth )
            , ( secondX, secondWorth )
            , ( secondX, secondInvestment )
            , ( firstX, firstInvestment )
            ]
        ]

    else
        let
            ratio =
                abs firstDifference / (abs firstDifference + abs secondDifference)

            crossX =
                firstX + (secondX - firstX) * ratio

            crossValue =
                firstWorth + (secondWorth - firstWorth) * ratio
        in
        [ polygon (colorFor firstDifference)
            [ ( firstX, firstWorth ), ( crossX, crossValue ), ( firstX, firstInvestment ) ]
        , polygon (colorFor secondDifference)
            [ ( crossX, crossValue ), ( secondX, secondWorth ), ( secondX, secondInvestment ) ]
        ]


adjacentPairs : List value -> List ( value, value )
adjacentPairs values =
    case values of
        first :: second :: rest ->
            ( first, second ) :: adjacentPairs (second :: rest)

        _ ->
            []


type alias ChartLayout =
    { height : Int
    , axisWidth : Int
    , plotTop : Float
    , plotBottom : Float
    , plotHeight : Float
    , plotWidth : Float
    , slotWidth : Float
    , barWidth : Float
    }


chartLayout : Int -> Int -> Int -> Int -> ChartLayout
chartLayout availableWidth count height targetSlotWidth =
    let
        axisWidth =
            68

        rightMargin =
            12

        plotTop =
            18

        plotBottomMargin =
            42

        visibleWidth =
            max 220 (availableWidth - axisWidth - rightMargin)

        visiblePeriods =
            clamp 5 30 (visibleWidth // targetSlotWidth)

        displayPeriods =
            min visiblePeriods (max 1 count)

        slotWidth =
            min (toFloat visibleWidth / toFloat displayPeriods) (toFloat targetSlotWidth)

        plotWidth =
            max (toFloat visibleWidth) (slotWidth * toFloat (max 1 count))
    in
    { height = height
    , axisWidth = axisWidth
    , plotTop = toFloat plotTop
    , plotBottom = toFloat (height - plotBottomMargin)
    , plotHeight = toFloat (height - plotTop - plotBottomMargin)
    , plotWidth = plotWidth
    , slotWidth = slotWidth
    , barWidth = min 18 (slotWidth * 0.64)
    }


fixedAxisChart :
    Theme
    -> String
    -> ChartLayout
    -> List Float
    -> (Float -> Float)
    -> Svg.Svg msg
    -> Element msg
fixedAxisChart theme commodity layout ticks y plot =
    let
        axis =
            Svg.svg
                [ SvgA.viewBox
                    ("0 0 " ++ String.fromInt layout.axisWidth ++ " " ++ String.fromInt layout.height)
                , SvgA.width (String.fromInt layout.axisWidth)
                , SvgA.height (String.fromInt layout.height)
                , SvgA.preserveAspectRatio "none"
                , Attr.attribute "aria-hidden" "true"
                ]
                (List.map
                    (\tick ->
                        Svg.text_
                            [ SvgA.x (String.fromInt (layout.axisWidth - 8))
                            , SvgA.y (float (y tick + 4))
                            , SvgA.textAnchor "end"
                            , SvgA.fill (Css.toCssColor theme.textMuted)
                            , SvgA.fontSize "11"
                            ]
                            [ Svg.text (axisMoney commodity tick) ]
                    )
                    ticks
                )
    in
    Element.el [ Element.width Element.fill, Typography.minWidthZero ]
        (Element.html
            (Html.div
                [ Attr.style "display" "grid"
                , Attr.style "grid-template-columns"
                    (String.fromInt layout.axisWidth ++ "px minmax(0, 1fr)")
                , Attr.style "width" "100%"
                , Attr.style "min-width" "0"
                ]
                [ axis
                , Html.div
                    [ Attr.style "width" "100%"
                    , Attr.style "min-width" "0"
                    , Attr.style "overflow-x" "auto"
                    , Attr.style "overflow-y" "hidden"
                    , Attr.style "-webkit-overflow-scrolling" "touch"
                    ]
                    [ plot ]
                ]
            )
        )


plotSvgAttributes : ChartLayout -> String -> List (Svg.Attribute msg)
plotSvgAttributes layout label =
    [ SvgA.viewBox ("0 0 " ++ float layout.plotWidth ++ " " ++ String.fromInt layout.height)
    , SvgA.width (float layout.plotWidth)
    , SvgA.height (String.fromInt layout.height)
    , SvgA.preserveAspectRatio "none"
    , Attr.attribute "role" "img"
    , Attr.attribute "aria-label" label
    , Attr.style "display" "block"
    , Attr.style "max-width" "none"
    ]


gridLines : Theme -> ChartLayout -> (Float -> Float) -> List Float -> List (Svg.Svg msg)
gridLines theme layout y ticks =
    List.map
        (\tick ->
            Svg.line
                [ SvgA.x1 "0"
                , SvgA.x2 (float layout.plotWidth)
                , SvgA.y1 (float (y tick))
                , SvgA.y2 (float (y tick))
                , SvgA.stroke (Css.toCssColor theme.divider)
                , SvgA.strokeWidth
                    (if tick == 0 then
                        "1.5"

                     else
                        "1"
                    )
                ]
                []
        )
        ticks


activeBand : Theme -> ChartLayout -> Bool -> Float -> Svg.Svg msg
activeBand theme layout selected center =
    if selected then
        Svg.rect
            [ SvgA.x (float (center - layout.slotWidth * 0.45))
            , SvgA.y (float layout.plotTop)
            , SvgA.width (float (layout.slotWidth * 0.9))
            , SvgA.height (float layout.plotHeight)
            , SvgA.rx "8"
            , SvgA.fill (Css.toCssColor theme.selectedBg)
            ]
            []

    else
        Svg.g [] []


roundedBar : Element.Color -> Float -> Float -> Float -> Float -> Float -> Svg.Svg msg
roundedBar color barWidth center zeroY valueY value =
    let
        top =
            min zeroY valueY

        height =
            max 0.5 (abs (zeroY - valueY))
    in
    Svg.rect
        [ SvgA.x (float (center - barWidth / 2))
        , SvgA.y (float top)
        , SvgA.width (float barWidth)
        , SvgA.height (float height)
        , SvgA.rx "2"
        , SvgA.fill (Css.toCssColor color)
        , SvgA.opacity
            (if abs value < 0.0001 then
                "0"

             else
                "0.9"
            )
        ]
        []


xAxisLabel : Theme -> ChartLayout -> Float -> String -> Svg.Svg msg
xAxisLabel theme layout center label =
    Svg.text_
        [ SvgA.x (float center)
        , SvgA.y (float (layout.plotBottom + 28))
        , SvgA.textAnchor "middle"
        , SvgA.fill (Css.toCssColor theme.textMuted)
        , SvgA.fontSize "11"
        ]
        [ Svg.text label ]


chartHitTarget : ChartLayout -> Float -> String -> String -> msg -> Svg.Svg msg
chartHitTarget layout center period label onSelect =
    Svg.rect
        [ SvgA.x (float (center - layout.slotWidth / 2))
        , SvgA.y (float layout.plotTop)
        , SvgA.width (float layout.slotWidth)
        , SvgA.height (float layout.plotHeight)
        , SvgA.fill "transparent"
        , SvgA.cursor "pointer"
        , Attr.tabindex 0
        , Attr.attribute "data-period" period
        , Attr.attribute "aria-label" label
        , Attr.attribute "role" "button"
        , SvgE.onClick onSelect
        ]
        []


performanceMarker : Theme -> Element.Color -> Float -> Float -> Svg.Svg msg
performanceMarker theme color x y =
    Svg.circle
        [ SvgA.cx (float x)
        , SvgA.cy (float y)
        , SvgA.r "4.5"
        , SvgA.fill (Css.toCssColor theme.cardBg)
        , SvgA.stroke (Css.toCssColor color)
        , SvgA.strokeWidth "3"
        ]
        []


categoryCard :
    Theme
    ->
        { title : String
        , commodity : String
        , breakdown : CategoryBreakdown
        , palette : Palette
        , otherExpanded : Bool
        , onOtherToggle : msg
        , highlightedCategory : Maybe Int
        , onSliceClick : Int -> msg
        }
    -> Element msg
categoryCard theme config =
    Ui.cardWith [ Element.width (Element.fillPortion 1), Element.alignTop ]
        theme
        [ Typography.subsectionTitle theme config.title
        , if List.isEmpty config.breakdown.items then
            Ui.emptyState theme "No categories for this period"

          else
            Element.wrappedRow
                [ Element.width Element.fill
                , Element.spacing 24
                , Element.paddingEach { top = 12, right = 0, bottom = 0, left = 0 }
                ]
                [ donut theme
                    config.palette
                    config.highlightedCategory
                    config.onSliceClick
                    config.breakdown.items
                , Element.column [ Element.width (Element.fillPortion 1), Element.spacing 12 ]
                    (categoryRows theme config)
                ]
        ]


depthControl : Theme -> Int -> (Int -> msg) -> Element msg
depthControl theme selected onDepth =
    Ui.segmentedControl theme
        (List.map
            (\depth ->
                Ui.segmentedOption theme
                    { label = String.fromInt depth
                    , selected = selected == depth
                    , onPress = onDepth depth
                    }
            )
            [ 1, 2, 3, 4 ]
        )


donut :
    Theme
    -> Palette
    -> Maybe Int
    -> (Int -> msg)
    -> List { item | account : String, quantity : Decimal }
    -> Element msg
donut theme palette highlightedCategory onSliceClick items =
    let
        values =
            List.map (.quantity >> decimalFloat >> abs) items

        radius =
            108

        pieData =
            Shape.pie
                { outerRadius = radius
                , innerRadius = 64
                , padAngle = 0.035
                , cornerRadius = 3
                , startAngle = 0
                , endAngle = 2 * pi
                , padRadius = 0
                , valueFn = identity
                , sortingFn = \_ _ -> EQ
                }
                values

        sliceColor index item =
            if item.account == "Other" then
                otherColor theme

            else
                paletteColor theme palette index

        renderSlice index item arc =
            let
                isSelected =
                    highlightedCategory == Just index

                midAngle =
                    (arc.startAngle + arc.endAngle) / 2

                tx =
                    if isSelected then
                        18 * cos midAngle

                    else
                        0

                ty =
                    if isSelected then
                        18 * sin midAngle

                    else
                        0

                sliceLabel =
                    categoryLabel item.account
            in
            Svg.g
                [ SvgA.transform
                    ("translate "
                        ++ String.fromFloat tx
                        ++ " "
                        ++ String.fromFloat ty
                    )
                , SvgA.cursor "pointer"
                , SvgA.style "transition: transform 150ms ease"
                , SvgE.onClick (onSliceClick index)
                , Attr.tabindex 0
                , Attr.attribute "role" "button"
                , Attr.attribute "aria-label"
                    (sliceLabel ++ " slice")
                ]
                [ Path.element (Shape.arc arc)
                    [ SvgA.fill
                        (Css.toCssColor (sliceColor index item))
                    , SvgA.stroke (Css.toCssColor theme.cardBg)
                    , SvgA.strokeWidth
                        (if isSelected then
                            "3"

                         else
                            "1"
                        )
                    ]
                ]

        indexedSlices =
            List.map2 Tuple.pair items pieData
                |> List.indexedMap
                    (\index ( item, arc ) ->
                        ( index, item, arc )
                    )

        ( selectedSlices, unselectedSlices ) =
            List.partition
                (\( index, _, _ ) -> highlightedCategory == Just index)
                indexedSlices
    in
    Element.el
        [ Element.width (Element.px 240)
        , Element.height (Element.px 240)
        ]
        (Element.html
            (Svg.svg
                [ SvgA.viewBox "0 0 240 240"
                , SvgA.width "240"
                , SvgA.height "240"
                , SvgA.style "overflow: visible"
                , Attr.attribute "role" "img"
                , Attr.attribute "aria-label"
                    "Category breakdown"
                ]
                [ Svg.g
                    [ SvgA.transform "translate(120,120)" ]
                    (List.map
                        (\( index, item, arc ) ->
                            renderSlice index item arc
                        )
                        (unselectedSlices ++ selectedSlices)
                    )
                ]
            )
        )


categoryRow :
    Theme
    -> String
    -> Palette
    -> Decimal
    -> Int
    -> Bool
    -> { item | account : String, quantity : Decimal }
    -> Element msg
categoryRow theme commodity palette total index isHighlighted item =
    let
        totalValue =
            decimalFloat total

        share =
            if totalValue <= 0 then
                0

            else
                decimalFloat item.quantity / totalValue * 100

        itemColor =
            if item.account == "Other" then
                otherColor theme

            else
                paletteColor theme palette index
    in
    Element.row
        [ Element.width Element.fill
        , Element.spacing 8
        , Element.paddingXY 4 2
        , Border.rounded 4
        , Background.color (Element.rgba 0 0 0 0)
        , Border.color
            (if isHighlighted then
                itemColor

             else
                Element.rgba 0 0 0 0
            )
        , Border.width 1
        ]
        [ Icon.view
            { icon = AccountPresentation.iconForAccountName item.account
            , size = 16
            , color = itemColor
            }
        , Element.el
            [ Element.width Element.fill
            , Typography.minWidthZero
            ]
            (Element.text (categoryLabel item.account))
        , Element.el
            [ Font.color theme.textMuted
            , Typography.financeNumber
            ]
            (Element.text
                (money commodity item.quantity
                    ++ " ("
                    ++ Formatting.roundedString share 0
                    ++ "%)"
                )
            )
        ]


categoryRows :
    Theme
    ->
        { config
            | commodity : String
            , breakdown : CategoryBreakdown
            , palette : Palette
            , otherExpanded : Bool
            , onOtherToggle : msg
            , highlightedCategory : Maybe Int
        }
    -> List (Element msg)
categoryRows theme config =
    let
        topLevel =
            config.breakdown.items
                |> List.indexedMap
                    (\index item ->
                        let
                            isHighlighted =
                                config.highlightedCategory
                                    == Just index
                        in
                        if item.account == "Other" && not (List.isEmpty config.breakdown.otherItems) then
                            Input.button
                                [ Element.width Element.fill
                                , Element.htmlAttribute
                                    (Attr.attribute "aria-expanded"
                                        (if config.otherExpanded then
                                            "true"

                                         else
                                            "false"
                                        )
                                    )
                                , Element.htmlAttribute
                                    (Attr.title
                                        (if config.otherExpanded then
                                            "Collapse aggregated categories"

                                         else
                                            "Expand aggregated categories"
                                        )
                                    )
                                ]
                                { onPress = Just config.onOtherToggle
                                , label =
                                    Element.row
                                        [ Element.width Element.fill
                                        , Element.spacing 6
                                        ]
                                        [ Element.el
                                            [ Element.width Element.fill ]
                                            (categoryRow theme
                                                config.commodity
                                                config.palette
                                                config.breakdown.total
                                                index
                                                isHighlighted
                                                item
                                            )
                                        , Element.text
                                            (if config.otherExpanded then
                                                "⌃"

                                             else
                                                "⌄"
                                            )
                                        ]
                                }

                        else
                            categoryRow theme
                                config.commodity
                                config.palette
                                config.breakdown.total
                                index
                                isHighlighted
                                item
                    )

        nested =
            if config.otherExpanded then
                config.breakdown.otherItems
                    |> List.indexedMap
                        (\index item ->
                            let
                                nestedIndex =
                                    List.length config.breakdown.items
                                        + index

                                isHighlighted =
                                    config.highlightedCategory
                                        == Just nestedIndex
                            in
                            Element.el
                                [ Element.width Element.fill
                                , Element.paddingEach
                                    { top = 0
                                    , right = 0
                                    , bottom = 0
                                    , left = 24
                                    }
                                ]
                                (categoryRow theme
                                    config.commodity
                                    config.palette
                                    config.breakdown.total
                                    nestedIndex
                                    isHighlighted
                                    item
                                )
                        )

            else
                []
    in
    topLevel ++ nested


activityMonth :
    Theme
    ->
        { title : String
        , period : String
        , commodity : String
        , tone : ActivityTone
        , response : Maybe ActivityResponse
        , selectedBucket : Maybe ActivityBucket
        , onSelect : ActivityBucket -> msg
        , onTransaction : Transaction -> msg
        , device : Device
        , legendState : LegendState
        , onLegendToggle : msg
        , highlightedCategory : Maybe Int
        , onCategoryHighlight : Int -> msg
        }
    -> Element msg
activityMonth theme config =
    let
        bucketFor day =
            let
                key =
                    dayKey config.period day
            in
            config.response
                |> Maybe.andThen (\response -> List.filter (.key >> (==) key) response.buckets |> List.head)

        categories =
            config.response
                |> Maybe.map activityCategories
                |> Maybe.withDefault []

        cell maybeDay =
            case maybeDay of
                Nothing ->
                    Element.el
                        [ Element.width (Element.fillPortion 1)
                        , Element.height (Element.px 72)
                        , equalCalendarColumn
                        ]
                        Element.none

                Just day ->
                    dayCell theme
                        config.tone
                        config.commodity
                        categories
                        config.highlightedCategory
                        (Maybe.map .key config.selectedBucket == Just (dayKey config.period day))
                        day
                        (bucketFor day)
                        config.onSelect

        rows =
            Calendar.monthCells config.period
                |> chunks 7
                |> List.map (List.map cell >> Element.row [ Element.width Element.fill ])
    in
    Ui.cardWith [ Element.width (Element.fillPortion 1), Element.alignTop ]
        theme
        [ Typography.subsectionTitle theme config.title
        , activityCategoryLegend theme
            config.tone
            categories
            config.legendState
            config.onLegendToggle
            config.highlightedCategory
            config.onCategoryHighlight
        , Element.row [ Element.width Element.fill ]
            (List.map
                (\label ->
                    Element.el
                        [ Element.width (Element.fillPortion 1)
                        , equalCalendarColumn
                        , Font.center
                        , Font.color theme.textMuted
                        ]
                        (Element.text label)
                )
                [ "S", "M", "T", "W", "T", "F", "S" ]
            )
        , Element.column [ Element.width Element.fill ] rows
        , case config.selectedBucket of
            Nothing ->
                Element.el [ Element.width Element.fill, Element.paddingXY 0 12 ]
                    (Typography.mutedCaption theme "Select a day to see details")

            Just bucket ->
                activityTransactionList theme
                    config.device
                    ("Transactions · " ++ bucket.key)
                    bucket.transactions
                    config.onTransaction
        ]


dayKey : String -> Int -> String
dayKey period day =
    period ++ "-" ++ String.padLeft 2 '0' (String.fromInt day)


dayCell :
    Theme
    -> ActivityTone
    -> String
    -> List BreakdownItem
    -> Maybe Int
    -> Bool
    -> Int
    -> Maybe ActivityBucket
    -> (ActivityBucket -> msg)
    -> Element msg
dayCell theme tone commodity categories highlightedCategory selected day maybeBucket onSelect =
    let
        label =
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing 8
                , Element.paddingXY 2 4
                ]
                [ Element.el [ Element.centerX ]
                    (dayRing theme tone categories highlightedCategory selected day maybeBucket)
                , maybeBucket
                    |> Maybe.map
                        (\bucket ->
                            Element.el
                                [ Element.centerX
                                , Font.size 10
                                , Font.semiBold
                                , Font.color (activityColor theme tone)
                                , Typography.financeNumber
                                ]
                                (Element.text (money commodity bucket.total))
                        )
                    |> Maybe.withDefault Element.none
                ]
    in
    case maybeBucket of
        Nothing ->
            Element.el
                [ Element.width (Element.fillPortion 1)
                , Element.height (Element.px 72)
                , equalCalendarColumn
                ]
                label

        Just bucket ->
            Input.button
                [ Element.width (Element.fillPortion 1)
                , Element.height (Element.px 72)
                , Border.rounded 8
                , equalCalendarColumn
                ]
                { onPress = Just (onSelect bucket), label = label }


equalCalendarColumn : Element.Attribute msg
equalCalendarColumn =
    Element.htmlAttribute (Attr.attribute "style" "flex: 1 1 0; min-width: 0")


activityCategories : ActivityResponse -> List BreakdownItem
activityCategories response =
    response.buckets
        |> List.concatMap .categories
        |> List.foldl
            (\category unique ->
                if List.any (.account >> (==) category.account) unique then
                    unique

                else
                    category :: unique
            )
            []
        |> List.sortBy .account


activityCategoryLegend :
    Theme
    -> ActivityTone
    -> List BreakdownItem
    -> LegendState
    -> msg
    -> Maybe Int
    -> (Int -> msg)
    -> Element msg
activityCategoryLegend theme tone categories legendState onToggle highlightedCategory onCategoryHighlight =
    if List.isEmpty categories then
        Element.none

    else
        let
            maxVisible =
                5

            isExpanded =
                legendState == LegendExpanded

            visibleCategories =
                if isExpanded || List.length categories <= maxVisible then
                    categories

                else
                    List.take maxVisible categories

            remainingCount =
                max 0 (List.length categories - maxVisible)
        in
        Element.column [ Element.width Element.fill, Element.spacing 10 ]
            [ Element.wrappedRow [ Element.width Element.fill, Element.spacing 10 ]
                (List.indexedMap
                    (\index category ->
                        let
                            isHighlighted =
                                highlightedCategory == Just index

                            dotColor =
                                if category.account == "Other" then
                                    otherColor theme

                                else
                                    activityCategoryColor theme tone index
                        in
                        Input.button
                            [ Element.spacing 5
                            , Element.paddingXY 4 2
                            , Border.rounded 4
                            , Background.color (Element.rgba 0 0 0 0)
                            , Border.color
                                (if isHighlighted then
                                    dotColor

                                 else
                                    Element.rgba 0 0 0 0
                                )
                            , Border.width 1
                            , Element.htmlAttribute
                                (Attr.style "cursor" "pointer")
                            ]
                            { onPress = Just (onCategoryHighlight index)
                            , label =
                                Element.row [ Element.spacing 5 ]
                                    [ Ui.accentDot dotColor
                                    , Typography.mutedCaption
                                        theme
                                        (categoryLabel category.account)
                                    ]
                            }
                    )
                    visibleCategories
                )
            , if remainingCount > 0 && not isExpanded then
                Input.button
                    [ Font.color theme.textMuted
                    ]
                    { onPress = Just onToggle
                    , label =
                        Typography.mutedCaption
                            theme
                            ("+"
                                ++ String.fromInt remainingCount
                                ++ " more"
                            )
                    }

              else if isExpanded && List.length categories > maxVisible then
                Input.button
                    [ Font.color theme.textMuted
                    ]
                    { onPress = Just onToggle
                    , label = Typography.mutedCaption theme "show less"
                    }

              else
                Element.none
            ]


dayRing :
    Theme
    -> ActivityTone
    -> List BreakdownItem
    -> Maybe Int
    -> Bool
    -> Int
    -> Maybe ActivityBucket
    -> Element msg
dayRing theme tone categories highlightedCategory selected day maybeBucket =
    let
        size =
            34

        gradient =
            maybeBucket
                |> Maybe.map
                    (\bucket ->
                        activityRingGradient
                            theme
                            tone
                            categories
                            highlightedCategory
                            bucket
                            (size // 2)
                    )
                |> Maybe.withDefault "transparent"
    in
    Element.html
        (Html.div
            [ Attr.style "width" (String.fromInt size ++ "px")
            , Attr.style "height" (String.fromInt size ++ "px")
            , Attr.style "border-radius" "50%"
            , Attr.style "background" gradient
            , Attr.style "display" "grid"
            , Attr.style "place-items" "center"
            , Attr.attribute "aria-hidden" "true"
            ]
            [ Html.div
                [ Attr.style "width" (String.fromInt (size - 8) ++ "px")
                , Attr.style "height" (String.fromInt (size - 8) ++ "px")
                , Attr.style "border-radius" "50%"
                , Attr.style "background"
                    (Css.toCssColor
                        (if selected then
                            theme.selectedBg

                         else
                            theme.cardBg
                        )
                    )
                , Attr.style "display" "grid"
                , Attr.style "place-items" "center"
                , Attr.style "font-size" "12px"
                , Attr.style "color" (Css.toCssColor theme.textPrimary)
                ]
                [ Html.text (String.fromInt day) ]
            ]
        )


miniDonut :
    Theme
    -> String
    -> ActivityTone
    -> List BreakdownItem
    -> Maybe Int
    -> Bool
    -> ActivityBucket
    -> (ActivityBucket -> msg)
    -> Element msg
miniDonut theme commodity tone categories highlightedCategory selected bucket onSelect =
    let
        gradient =
            activityRingGradient theme tone categories highlightedCategory bucket 45

        isEmpty =
            isEmptyActivityBucket bucket

        monthLabel =
            String.left 3 (periodName bucket.key)
    in
    Input.button
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.htmlAttribute (Attr.style "min-width" "70px")
        , Element.htmlAttribute (Attr.style "max-width" "110px")
        , Element.htmlAttribute (Attr.style "aspect-ratio" "1")
        ]
        { onPress = Just (onSelect bucket)
        , label =
            Element.html
                (Html.div
                    [ Attr.style "width" "100%"
                    , Attr.style "height" "100%"
                    , Attr.style "border-radius" "50%"
                    , Attr.style "background" gradient
                    , Attr.style "display" "grid"
                    , Attr.style "place-items" "center"
                    , Attr.style "cursor" "pointer"
                    , Attr.style "transition" "transform 0.15s ease"
                    , Attr.style "transform"
                        (if selected then
                            "scale(1.05)"

                         else
                            "scale(1)"
                        )
                    ]
                    [ Html.div
                        [ Attr.style "width" "calc(100% - 24px)"
                        , Attr.style "height" "calc(100% - 24px)"
                        , Attr.style "border-radius" "50%"
                        , Attr.style "background"
                            (Css.toCssColor
                                (if selected then
                                    theme.selectedBg

                                 else
                                    theme.cardBg
                                )
                            )
                        , Attr.style "display" "flex"
                        , Attr.style "flex-direction" "column"
                        , Attr.style "align-items" "center"
                        , Attr.style "justify-content" "center"
                        , Attr.style "gap" "2px"
                        ]
                        [ Html.div
                            [ Attr.style "font-size" "10px"
                            , Attr.style "font-weight" "600"
                            , Attr.style "color" (Css.toCssColor theme.textMuted)
                            , Attr.style "line-height" "1"
                            ]
                            [ Html.text monthLabel ]
                        , Html.div
                            [ Attr.style "font-size" "11px"
                            , Attr.style "font-weight" "600"
                            , Attr.style "color"
                                (Css.toCssColor
                                    (if isEmpty then
                                        theme.textMuted

                                     else
                                        activityColor theme tone
                                    )
                                )
                            , Attr.style "line-height" "1"
                            ]
                            [ Html.text (money commodity bucket.total) ]
                        ]
                    ]
                )
        }


activityRingGradient :
    Theme
    -> ActivityTone
    -> List BreakdownItem
    -> Maybe Int
    -> ActivityBucket
    -> Int
    -> String
activityRingGradient theme tone categories highlightedCategory bucket radius =
    let
        pieces =
            bucket.categories
                |> List.map
                    (\category ->
                        let
                            index =
                                categoryIndex category.account categories

                            baseColor =
                                if category.account == "Other" then
                                    otherColor theme

                                else
                                    activityCategoryColor theme tone index

                            color =
                                case highlightedCategory of
                                    Nothing ->
                                        baseColor

                                    Just highlightedIndex ->
                                        if highlightedIndex == index then
                                            baseColor

                                        else
                                            withAlpha 0.14 baseColor
                        in
                        ( color
                        , abs (decimalFloat category.quantity)
                        )
                    )

        total =
            List.sum (List.map Tuple.second pieces)

        gapPixels =
            2

        gapSize =
            gapPixels / (2 * pi * toFloat radius) * 100

        gapHalf =
            gapSize / 2

        step ( color, value ) ( offset, rendered ) =
            let
                next =
                    if total <= 0 then
                        offset

                    else
                        offset + value / total * 100

                segmentStart =
                    offset + gapHalf

                segmentEnd =
                    next - gapHalf

                colorHex =
                    Css.toCssColor color
            in
            ( next
            , if segmentEnd > segmentStart then
                rendered
                    ++ [ colorHex ++ " " ++ float segmentStart ++ "%"
                       , colorHex ++ " " ++ float segmentEnd ++ "%"
                       , "transparent " ++ float segmentEnd ++ "%"
                       , "transparent " ++ float (next + gapHalf) ++ "%"
                       ]

              else
                rendered
            )

        stops =
            pieces |> List.foldl step ( 0, [] ) |> Tuple.second
    in
    if isEmptyActivityBucket bucket then
        emptyActivityRingColor theme

    else if List.isEmpty stops then
        Css.toCssColor (activityColor theme tone)

    else
        "conic-gradient(" ++ String.join ", " stops ++ ")"


isEmptyActivityBucket : ActivityBucket -> Bool
isEmptyActivityBucket bucket =
    List.isEmpty bucket.categories && List.isEmpty bucket.transactions


emptyActivityRingColor : Theme -> String
emptyActivityRingColor theme =
    Css.toCssColor theme.stale


withAlpha : Float -> Element.Color -> Element.Color
withAlpha alpha color =
    let
        rgb =
            Element.toRgb color
    in
    Element.rgba rgb.red rgb.green rgb.blue alpha


categoryIndex : String -> List BreakdownItem -> Int
categoryIndex account categories =
    categories
        |> List.indexedMap Tuple.pair
        |> List.filter (Tuple.second >> .account >> (==) account)
        |> List.head
        |> Maybe.map Tuple.first
        |> Maybe.withDefault 0


activityCategoryColor : Theme -> ActivityTone -> Int -> Element.Color
activityCategoryColor theme tone index =
    paletteColor theme
        (case tone of
            IncomeTone ->
                PositivePalette

            ExpenseTone ->
                NegativePalette

            AssetTone ->
                AssetPalette

            LiabilityTone ->
                LiabilityPalette
        )
        index


activityTransactionList : Theme -> Device -> String -> List Transaction -> (Transaction -> msg) -> Element msg
activityTransactionList theme device title transactions onSelect =
    Element.column [ Element.width Element.fill, Element.spacing 8 ]
        [ Typography.subsectionTitle theme title
        , if List.isEmpty transactions then
            Typography.mutedCaption theme "No transactions for this period"

          else
            Element.column
                [ Element.width Element.fill
                , Element.spacing 4
                ]
                (List.map
                    (\transaction ->
                        Input.button (Ui.rowSurfaceAttrs theme False ++ [ Element.width Element.fill ])
                            { onPress = Just (onSelect transaction)
                            , label =
                                Element.row
                                    [ Element.width Element.fill
                                    , Element.spacing 14
                                    , Element.paddingXY 4 10
                                    ]
                                    [ Element.el [ Font.color theme.textMuted, Font.size 15 ]
                                        (Element.text (Date.toString transaction.primaryDate))
                                    , Element.el
                                        [ Element.width Element.fill
                                        , Typography.minWidthZero
                                        , Font.size 15
                                        ]
                                        (Element.text transaction.description)
                                    , Element.text "›"
                                    ]
                            }
                    )
                    transactions
                )
        ]


activityYear :
    Theme
    ->
        { title : String
        , commodity : String
        , tone : ActivityTone
        , response : Maybe ActivityResponse
        , selectedBucket : Maybe ActivityBucket
        , onSelect : ActivityBucket -> msg
        , device : Device
        , legendState : LegendState
        , onLegendToggle : msg
        , highlightedCategory : Maybe Int
        , onCategoryHighlight : Int -> msg
        }
    -> Element msg
activityYear theme config =
    Ui.cardWith [ Element.width (Element.fillPortion 1), Element.alignTop ]
        theme
        [ Typography.subsectionTitle theme config.title
        , case config.response of
            Nothing ->
                Ui.loading theme "Loading activity"

            Just response ->
                let
                    categories =
                        activityCategories response

                    selected =
                        config.selectedBucket

                    displayBuckets =
                        ActivityCalendar.yearBuckets response

                    cols =
                        case config.device.class of
                            Element.Phone ->
                                3

                            Element.Tablet ->
                                4

                            Element.Desktop ->
                                6

                            Element.BigDesktop ->
                                6

                    slots =
                        displayBuckets
                            |> List.map Just
                            |> padToMultiple cols Nothing
                in
                Element.column [ Element.width Element.fill, Element.spacing 12 ]
                    [ activityCategoryLegend theme
                        config.tone
                        categories
                        config.legendState
                        config.onLegendToggle
                        config.highlightedCategory
                        config.onCategoryHighlight
                    , Element.column [ Element.width Element.fill, Element.spacing 16 ]
                        (chunks cols slots
                            |> List.map
                                (\row ->
                                    Element.row [ Element.width Element.fill, Element.spacing 16 ]
                                        (List.map
                                            (\maybeBucket ->
                                                case maybeBucket of
                                                    Just bucket ->
                                                        Element.el
                                                            [ Element.width (Element.fillPortion 1)
                                                            , Element.paddingXY 0 8
                                                            ]
                                                            (miniDonut theme
                                                                config.commodity
                                                                config.tone
                                                                categories
                                                                config.highlightedCategory
                                                                (Maybe.map .key selected == Just bucket.key)
                                                                bucket
                                                                config.onSelect
                                                            )

                                                    Nothing ->
                                                        Element.el
                                                            [ Element.width (Element.fillPortion 1)
                                                            , Element.paddingXY 0 8
                                                            ]
                                                            Element.none
                                            )
                                            row
                                        )
                                )
                        )
                    , case selected of
                        Nothing ->
                            Element.el [ Element.width Element.fill, Element.paddingXY 0 12 ]
                                (Typography.mutedCaption theme "Select a month to see details")

                        Just bucket ->
                            activityYearLegend theme
                                config.commodity
                                config.tone
                                categories
                                bucket
                    ]
        ]


activityYearLegend :
    Theme
    -> String
    -> ActivityTone
    -> List BreakdownItem
    -> ActivityBucket
    -> Element msg
activityYearLegend theme commodity tone categories bucket =
    let
        total =
            bucket.categories
                |> List.map (.quantity >> decimalFloat >> abs)
                |> List.sum

        items =
            bucket.categories
                |> List.sortBy (.quantity >> decimalFloat >> abs >> negate)
    in
    Element.column [ Element.width Element.fill, Element.spacing 8 ]
        [ Element.row [ Element.width Element.fill, Element.spacing 8, Element.paddingXY 0 8 ]
            [ Typography.mutedBody theme (periodName bucket.key ++ " · ")
            , Element.el [ Font.color (activityColor theme tone), Font.semiBold, Typography.financeNumber ]
                (Element.text (money commodity bucket.total))
            ]
        , Element.column [ Element.width Element.fill, Element.spacing 4 ]
            (List.indexedMap
                (\index item ->
                    let
                        share =
                            if total <= 0 then
                                0

                            else
                                (decimalFloat item.quantity / total) * 100
                    in
                    Element.row [ Element.width Element.fill, Element.spacing 8 ]
                        [ Icon.view
                            { icon = AccountPresentation.iconForAccountName item.account
                            , size = 16
                            , color =
                                if item.account == "Other" then
                                    otherColor theme

                                else
                                    activityCategoryColor theme tone index
                            }
                        , Element.el [ Element.width Element.fill, Typography.minWidthZero ]
                            (Element.text (categoryLabel item.account))
                        , Element.el [ Font.color theme.textMuted, Typography.financeNumber ]
                            (Element.text
                                (money commodity item.quantity
                                    ++ " ("
                                    ++ Formatting.roundedString share 0
                                    ++ "%)"
                                )
                            )
                        ]
                )
                items
            )
        ]


activityTransactions : Theme -> String -> List Transaction -> (Transaction -> msg) -> Element msg
activityTransactions theme title transactions onSelect =
    Ui.card theme
        [ Typography.subsectionTitle theme title
        , if List.isEmpty transactions then
            Typography.mutedParagraph theme "No contributing transactions"

          else
            Element.column [ Element.width Element.fill ]
                (List.intersperse (Ui.hairline theme)
                    (List.map
                        (\transaction ->
                            Input.button [ Element.width Element.fill, Element.paddingXY 0 10 ]
                                { onPress = Just (onSelect transaction)
                                , label =
                                    Element.row [ Element.width Element.fill, Element.spacing 12 ]
                                        [ Element.el [ Font.color theme.textMuted ]
                                            (Element.text (Date.toString transaction.primaryDate))
                                        , Element.el [ Element.width Element.fill ]
                                            (Element.text transaction.description)
                                        , Element.text "›"
                                        ]
                                }
                        )
                        transactions
                    )
                )
        ]


warningList : Theme -> List String -> Element msg
warningList theme warnings =
    if List.isEmpty warnings then
        Element.none

    else
        Element.column [ Element.width Element.fill, Element.spacing 8 ]
            (List.map (Ui.errorBanner theme) warnings)


paletteColor : Theme -> Palette -> Int -> Element.Color
paletteColor theme palette index =
    let
        order =
            case palette of
                PositivePalette ->
                    [ 1, 7, 4, 3, 0, 2, 9, 5, 8 ]

                AssetPalette ->
                    [ 1, 7, 4, 3, 0, 2, 9, 5, 8 ]

                NegativePalette ->
                    [ 5, 8, 2, 0, 3, 4, 9, 1, 7 ]

                LiabilityPalette ->
                    [ 5, 8, 2, 0, 3, 4, 9, 1, 7 ]

        colorIndex =
            order |> List.drop (modBy (List.length order) index) |> List.head |> Maybe.withDefault 0
    in
    theme.chartSeries |> List.drop colorIndex |> List.head |> Maybe.withDefault theme.primary


otherColor : Theme -> Element.Color
otherColor theme =
    theme.chartSeries |> List.drop 6 |> List.head |> Maybe.withDefault theme.primary


activityColor : Theme -> ActivityTone -> Element.Color
activityColor theme tone =
    case tone of
        IncomeTone ->
            theme.income

        ExpenseTone ->
            theme.liability

        AssetTone ->
            theme.positive

        LiabilityTone ->
            theme.negative


money : String -> Decimal -> String
money commodity amount =
    Formatting.formatAmountDisplay (Formatting.commodityDisplaySymbol commodity) (decimalFloat amount)


moneyFloat : String -> Float -> String
moneyFloat commodity amount =
    Formatting.formatAmountDisplay (Formatting.commodityDisplaySymbol commodity) amount


decimalFloat : Decimal -> Float
decimalFloat amount =
    Decimal.toString amount |> String.toFloat |> Maybe.withDefault 0


axisMoney : String -> Float -> String
axisMoney commodity amount =
    let
        ( value, suffix ) =
            Formatting.compactValue (Formatting.commodityDisplaySymbol commodity) (abs amount)

        sign =
            if amount < 0 then
                "-"

            else
                ""
    in
    sign ++ value ++ suffix


periodName : String -> String
periodName period =
    if String.length period == 7 then
        Calendar.periodLabel Monthly period

    else
        period


shortPeriod : String -> String
shortPeriod period =
    if String.length period == 7 then
        String.right 2 period

    else
        period


categoryLabel : String -> String
categoryLabel account =
    if account == "Other" then
        "Others (agg.)"

    else
        case String.split ":" account of
            _ :: nested ->
                if List.isEmpty nested then
                    account

                else
                    String.join ":" nested

            [] ->
                account


float : Float -> String
float =
    String.fromFloat


chunks : Int -> List value -> List (List value)
chunks size values =
    if List.isEmpty values then
        []

    else
        List.take size values :: chunks size (List.drop size values)


padToMultiple : Int -> value -> List value -> List value
padToMultiple size padding values =
    let
        missing =
            modBy size (size - modBy size (List.length values))
    in
    values ++ List.repeat missing padding
