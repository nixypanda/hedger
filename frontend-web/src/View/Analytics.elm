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
    , netWorthPerformanceChart
    , positiveNegativeTimelineChart
    , warningList
    )

import Data.AccountType exposing (AccountType)
import Data.ActivityCalendar as ActivityCalendar
import Data.Analytics
    exposing
        ( ActivityBucket
        , ActivityDirection(..)
        , ActivityResponse
        , BreakdownItem
        , CategoryBreakdown
        , Kpi
        , PerformancePoint
        , TimelinePoint
        )
import Data.Calendar as Calendar
import Data.Date as Date
import Data.Decimal as Decimal exposing (Decimal)
import Data.Granularity exposing (Granularity(..))
import Data.Transaction exposing (Transaction)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attr
import List
import Ui
import Ui.Calendar.Month as MonthCalendar
import Ui.Chart as Chart
import Ui.Chart.Cartesian as Cartesian
import Ui.Chart.Donut as Donut
import Ui.Chart.Ring as Ring
import Ui.Chart.Sparkline as Sparkline
import Ui.DesignTokens as Tokens
import Ui.Icon as Icon exposing (Icon)
import Ui.Theme as Theme exposing (Theme)
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
        [ Element.width (Element.fillPortion Tokens.layoutPortionSingle)
        , Element.height (Element.px Tokens.analyticsChartHeight)
        ]
        theme
        [ Element.row [ Element.width Element.fill, Element.spacing Tokens.space12 ]
            [ Element.el
                [ Element.width (Element.px Tokens.analyticsRingSize)
                , Element.height (Element.px Tokens.analyticsRingSize)
                , Background.color theme.primarySoft
                , Border.rounded Tokens.radiusMedium
                ]
                (Element.el [ Element.centerX, Element.centerY ]
                    (Icon.view
                        { icon = config.icon
                        , size = Tokens.iconSizeDetail
                        , color = config.color
                        }
                    )
                )
            , kpiTrendSparkline config.color (List.map decimalFloat config.kpi.trend)
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


kpiTrendSparkline : Element.Color -> List Float -> Element msg
kpiTrendSparkline color values =
    Sparkline.view
        { width = 126
        , height = 48
        , color = color
        , label = "Recent trend"
        , values = values
        }


positiveNegativeTimelineChart :
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
positiveNegativeTimelineChart theme config =
    let
        selectedPoint =
            config.selected
                |> Maybe.andThen
                    (\period -> List.filter (.period >> (==) period) config.points |> List.head)

        chart =
            Cartesian.view theme
                { availableWidth = config.availableWidth
                , height = 300
                , targetSlotWidth = 40
                , label = "Assets and liabilities timeline"
                , items = config.points
                , key = .period
                , xLabel = .period >> shortPeriod >> Just
                , describe =
                    \point ->
                        point.period
                            ++ ": "
                            ++ String.toLower config.positiveLabel
                            ++ " "
                            ++ money config.commodity point.positive
                            ++ ", "
                            ++ String.toLower config.negativeLabel
                            ++ " "
                            ++ money config.commodity point.negative
                , selected = config.selected
                , onSelect = config.onSelect
                , formatTick = axisMoney config.commodity
                , domain = Cartesian.SymmetricAroundZero
                , series =
                    [ Cartesian.Bars
                        { value = .positive >> decimalFloat
                        , baseline = 0
                        , color = theme.positive
                        }
                    , Cartesian.Bars
                        { value = .negative >> decimalFloat >> abs >> negate
                        , baseline = 0
                        , color = theme.negative
                        }
                    , Cartesian.Line
                        { value = .net >> decimalFloat >> Just
                        , color = theme.primary
                        , width = 2.6
                        , markers = Cartesian.SelectedMarkers
                        }
                    ]
                }
    in
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space12 ]
        [ chart
        , selectedPoint
            |> Maybe.map
                (\point ->
                    selectedPeriodDetails theme
                        (periodName point.period)
                        [ { label = config.positiveLabel
                          , value = money config.commodity point.positive
                          , color = theme.positive
                          }
                        , { label = config.negativeLabel
                          , value = money config.commodity point.negative
                          , color = theme.negative
                          }
                        , { label = config.netLabel
                          , value = money config.commodity point.net
                          , color = theme.primary
                          }
                        ]
                )
            |> Maybe.withDefault Element.none
        ]


netWorthPerformanceChart :
    Theme
    ->
        { commodity : String
        , points : List PerformancePoint
        , selected : Maybe String
        , onSelect : String -> msg
        , availableWidth : Int
        }
    -> Element msg
netWorthPerformanceChart theme config =
    let
        selectedPoint =
            config.selected
                |> Maybe.andThen
                    (\period -> List.filter (.period >> (==) period) config.points |> List.head)

        chart =
            Cartesian.view theme
                { availableWidth = config.availableWidth
                , height = 250
                , targetSlotWidth = 52
                , label = "Net investment and net worth timeline"
                , items = config.points
                , key = .period
                , xLabel = .period >> shortPeriod >> Just
                , describe =
                    \point ->
                        point.period
                            ++ ": net investment "
                            ++ money config.commodity point.netInvestment
                            ++ ", net worth "
                            ++ money config.commodity point.netWorth
                , selected = config.selected
                , onSelect = config.onSelect
                , formatTick = axisMoney config.commodity
                , domain = Cartesian.PaddedExtent { ratio = 0.12, minimum = 1 }
                , series =
                    [ Cartesian.AreaBetween
                        { first = .netWorth >> decimalFloat >> Just
                        , second = .netInvestment >> decimalFloat >> Just
                        , positiveColor = theme.positive
                        , negativeColor = theme.negative
                        , opacity = 0.18
                        }
                    , Cartesian.Line
                        { value = .netWorth >> decimalFloat >> Just
                        , color = theme.primary
                        , width = 2.6
                        , markers = Cartesian.SelectedMarkers
                        }
                    , Cartesian.Line
                        { value = .netInvestment >> decimalFloat >> Just
                        , color = theme.positive
                        , width = 2.4
                        , markers = Cartesian.SelectedMarkers
                        }
                    ]
                }
    in
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space12 ]
        [ chart
        , selectedPoint
            |> Maybe.map
                (\point ->
                    let
                        gainLoss =
                            decimalFloat point.gainLoss

                        gainLossLabel =
                            if gainLoss >= 0 then
                                "Gain"

                            else
                                "Loss"

                        gainLossColor =
                            if gainLoss >= 0 then
                                theme.positive

                            else
                                theme.negative
                    in
                    selectedPeriodDetails theme
                        (periodName point.period)
                        [ { label = "Net investment"
                          , value = money config.commodity point.netInvestment
                          , color = theme.positive
                          }
                        , { label = "Net worth"
                          , value = money config.commodity point.netWorth
                          , color = theme.primary
                          }
                        , { label = gainLossLabel
                          , value = moneyFloat config.commodity (abs gainLoss)
                          , color = gainLossColor
                          }
                        ]
                )
            |> Maybe.withDefault Element.none
        ]


selectedPeriodDetails :
    Theme
    -> String
    -> List { label : String, value : String, color : Element.Color }
    -> Element msg
selectedPeriodDetails theme period metrics =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Tokens.space8
        , Element.padding Tokens.space12
        , Background.color theme.primarySoft
        ]
        [ Element.el [ Font.semiBold ] (Element.text period)
        , Element.wrappedRow
            [ Element.width Element.fill
            , Element.spacing Tokens.space18
            ]
            (List.map (selectedPeriodMetric theme) metrics)
        ]


selectedPeriodMetric :
    Theme
    -> { label : String, value : String, color : Element.Color }
    -> Element msg
selectedPeriodMetric theme metric =
    Element.column
        [ Element.spacing Tokens.space4
        , Element.width (Element.fillPortion Tokens.layoutPortionSingle)
        ]
        [ Typography.mutedCaption theme metric.label
        , Element.el
            [ Typography.financeNumber
            , Font.semiBold
            , Font.color metric.color
            ]
            (Element.text metric.value)
        ]


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
    Ui.cardWith
        [ Element.width (Element.fillPortion Tokens.layoutPortionSingle)
        , Element.alignTop
        , Element.height Element.fill
        ]
        theme
        [ Typography.subsectionTitle theme config.title
        , if List.isEmpty config.breakdown.items then
            Ui.emptyState theme "No categories for this period"

          else
            Element.wrappedRow
                [ Element.width Element.fill
                , Element.spacing Tokens.space24
                , Element.paddingEach
                    { top = Tokens.space12
                    , right = Tokens.space0
                    , bottom = Tokens.space0
                    , left = Tokens.space0
                    }
                ]
                [ categoryBreakdownDonut theme
                    config.palette
                    config.highlightedCategory
                    config.onSliceClick
                    config.breakdown.items
                , Element.column
                    [ Element.width (Element.fillPortion Tokens.layoutPortionSingle)
                    , Element.spacing Tokens.space12
                    , Element.alignTop
                    ]
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


categoryBreakdownDonut :
    Theme
    -> Palette
    -> Maybe Int
    -> (Int -> msg)
    -> List { item | account : String, quantity : Decimal }
    -> Element msg
categoryBreakdownDonut theme palette highlightedCategory onSliceClick items =
    let
        sliceColor index item =
            if item.account == "Other" then
                otherColor theme

            else
                paletteColor theme palette index
    in
    Element.el [ Element.alignTop ]
        (Donut.view
            { size = 240
            , outerRadius = 108
            , innerRadius = 64
            , padAngle = 0.035
            , cornerRadius = 3
            , selectedOffset = 18
            , label = "Category breakdown"
            , items = items
            , value = .quantity >> decimalFloat
            , color = sliceColor
            , itemLabel = .account >> categoryLabel >> (\label -> label ++ " slice")
            , selected = highlightedCategory
            , onSelect = onSliceClick
            , backgroundColor = theme.cardBg
            }
        )


categoryRow :
    Theme
    -> String
    -> Palette
    -> Decimal
    -> Int
    -> Bool
    -> { item | account : String, accountType : Maybe AccountType, quantity : Decimal }
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
        , Element.spacing Tokens.space8
        , Element.paddingXY Tokens.space4 Tokens.space2
        , Border.rounded Tokens.radiusTight
        , Background.color theme.transparent
        , Border.color
            (if isHighlighted then
                itemColor

             else
                theme.transparent
            )
        , Border.width Tokens.borderHairline
        ]
        [ Icon.view
            { icon = AccountPresentation.iconForAccount item.accountType item.account
            , size = Tokens.iconSizeSmallGlyph
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
                        if
                            item.account
                                == "Other"
                                && not (List.isEmpty config.breakdown.otherItems)
                        then
                            Input.button
                                [ Element.width Element.fill
                                , Ui.touchTargetHeight
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
                                        , Element.spacing Tokens.space6
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
                                    { top = Tokens.space0
                                    , right = Tokens.space0
                                    , bottom = Tokens.space0
                                    , left = Tokens.space24
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
                |> Maybe.andThen
                    (\response ->
                        List.filter (.key >> (==) key) response.buckets
                            |> List.head
                    )

        categories =
            config.response
                |> Maybe.map activityCategories
                |> Maybe.withDefault []

        calendar =
            MonthCalendar.view
                { period = config.period
                , weekdayLabels = [ "S", "M", "T", "W", "T", "F", "S" ]
                , weekdayColor = theme.textMuted
                , cellHeight = 72
                , viewDay =
                    \day attributes ->
                        dayCell
                            { attributes = attributes
                            , theme = theme
                            , tone = config.tone
                            , commodity = config.commodity
                            , categories = categories
                            , highlightedCategory = config.highlightedCategory
                            , selected =
                                Maybe.map .key config.selectedBucket
                                    == Just (dayKey config.period day)
                            , day = day
                            , maybeBucket = bucketFor day
                            , onSelect = config.onSelect
                            }
                }
    in
    Ui.cardWith [ Element.width (Element.fillPortion Tokens.layoutPortionSingle), Element.alignTop ]
        theme
        [ Typography.subsectionTitle theme config.title
        , activityLegend theme
            config.tone
            categories
            config.legendState
            config.onLegendToggle
            config.highlightedCategory
            config.onCategoryHighlight
        , calendar
        , case config.selectedBucket of
            Nothing ->
                Element.el
                    [ Element.width Element.fill
                    , Element.paddingXY Tokens.space0 Tokens.space12
                    ]
                    (Typography.mutedCaption theme "Select a day to see details")

            Just bucket ->
                activityTransactionList theme
                    ("Transactions · " ++ bucket.key)
                    bucket.transactions
                    config.onTransaction
        ]


dayKey : String -> Int -> String
dayKey period day =
    period ++ "-" ++ String.padLeft 2 '0' (String.fromInt day)


type alias DayCellConfig msg =
    { attributes : List (Element.Attribute msg)
    , theme : Theme
    , tone : ActivityTone
    , commodity : String
    , categories : List BreakdownItem
    , highlightedCategory : Maybe Int
    , selected : Bool
    , day : Int
    , maybeBucket : Maybe ActivityBucket
    , onSelect : ActivityBucket -> msg
    }


dayCell : DayCellConfig msg -> Element msg
dayCell config =
    let
        attributes =
            config.attributes

        theme =
            config.theme

        tone =
            config.tone

        commodity =
            config.commodity

        categories =
            config.categories

        highlightedCategory =
            config.highlightedCategory

        selected =
            config.selected

        day =
            config.day

        maybeBucket =
            config.maybeBucket

        onSelect =
            config.onSelect

        label =
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing Tokens.space8
                , Element.paddingXY Tokens.space2 Tokens.space4
                ]
                [ Element.el [ Element.centerX ]
                    (activityDayRing
                        theme
                        tone
                        categories
                        highlightedCategory
                        selected
                        day
                        maybeBucket
                    )
                , maybeBucket
                    |> Maybe.map
                        (\bucket ->
                            Element.el
                                [ Element.centerX
                                , Typography.caption
                                , Font.semiBold
                                , Font.color (ringAccent theme tone bucket)
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
                attributes
                label

        Just bucket ->
            Input.button
                (Border.rounded Tokens.radiusSmall :: attributes)
                { onPress = Just (onSelect bucket), label = label }


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


type alias ActivityCategoryLegendConfig msg =
    { theme : Theme
    , tone : ActivityTone
    , categories : List BreakdownItem
    , legendState : LegendState
    , onToggle : msg
    , highlightedCategory : Maybe Int
    , onCategoryHighlight : Int -> msg
    }


activityCategoryLegend : ActivityCategoryLegendConfig msg -> Element msg
activityCategoryLegend config =
    let
        theme =
            config.theme

        tone =
            config.tone

        categories =
            config.categories

        legendState =
            config.legendState

        onToggle =
            config.onToggle

        highlightedCategory =
            config.highlightedCategory

        onCategoryHighlight =
            config.onCategoryHighlight
    in
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
        Element.column [ Element.width Element.fill, Element.spacing Tokens.space10 ]
            [ Element.wrappedRow [ Element.width Element.fill, Element.spacing Tokens.space10 ]
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
                        Ui.legendButton theme
                            { accent = dotColor
                            , selected = isHighlighted
                            , onPress = onCategoryHighlight index
                            , label =
                                Element.row [ Element.spacing Tokens.space5 ]
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
                Ui.legendToggleButton theme
                    { onPress = onToggle
                    , label =
                        Typography.mutedCaption
                            theme
                            ("+"
                                ++ String.fromInt remainingCount
                                ++ " more"
                            )
                    }

              else if isExpanded && List.length categories > maxVisible then
                Ui.legendToggleButton theme
                    { onPress = onToggle
                    , label = Typography.mutedCaption theme "show less"
                    }

              else
                Element.none
            ]


activityDayRing :
    Theme
    -> ActivityTone
    -> List BreakdownItem
    -> Maybe Int
    -> Bool
    -> Int
    -> Maybe ActivityBucket
    -> Element msg
activityDayRing theme tone categories highlightedCategory selected day maybeBucket =
    let
        size =
            34

        segments =
            maybeBucket
                |> Maybe.map
                    (activityRingSegments theme tone categories highlightedCategory)
                |> Maybe.withDefault []

        emptyColor =
            case maybeBucket of
                Nothing ->
                    theme.transparent

                Just bucket ->
                    if isEmptyActivityBucket bucket then
                        theme.stale

                    else
                        ringAccent theme tone bucket
    in
    Ring.view
        { size = Ring.Pixels size
        , radius = toFloat size / 2
        , thickness = 4
        , gap = 2
        , segments = segments
        , emptyColor = emptyColor
        , centerColor =
            if selected then
                theme.selectedBg

            else
                theme.cardBg
        , selected = False
        , onPress = Nothing
        , label = Nothing
        , center =
            Chart.ringDayLabel theme (String.fromInt day)
        }


activityMonthRing :
    Theme
    -> String
    -> ActivityTone
    -> List BreakdownItem
    -> Maybe Int
    -> Bool
    -> ActivityBucket
    -> (ActivityBucket -> msg)
    -> Element msg
activityMonthRing theme commodity tone categories highlightedCategory selected bucket onSelect =
    let
        isEmpty =
            isEmptyActivityBucket bucket

        monthLabel =
            String.left 3 (periodName bucket.key)
    in
    Ring.view
        { size = Ring.Fill { minimum = 70, maximum = 110 }
        , radius = 45
        , thickness = 12
        , gap = 2
        , segments = activityRingSegments theme tone categories highlightedCategory bucket
        , emptyColor =
            if isEmpty then
                theme.stale

            else
                ringAccent theme tone bucket
        , centerColor =
            if selected then
                theme.selectedBg

            else
                theme.cardBg
        , selected = selected
        , onPress = Just (onSelect bucket)
        , label = Just (periodName bucket.key ++ " " ++ money commodity bucket.total)
        , center =
            Chart.ringMonthCenter theme
                monthLabel
                (monthRingCenter theme tone commodity isEmpty bucket)
        }


activityRingSegments :
    Theme
    -> ActivityTone
    -> List BreakdownItem
    -> Maybe Int
    -> ActivityBucket
    -> List Ring.Segment
activityRingSegments theme tone categories highlightedCategory bucket =
    case directionSpecsFor theme tone of
        Just specs ->
            directionSegments specs highlightedCategory bucket

        Nothing ->
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
                        { color = color
                        , value = abs (decimalFloat category.quantity)
                        }
                    )


isEmptyActivityBucket : ActivityBucket -> Bool
isEmptyActivityBucket bucket =
    List.isEmpty bucket.categories && List.isEmpty bucket.transactions


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


activityTransactionList : Theme -> String -> List Transaction -> (Transaction -> msg) -> Element msg
activityTransactionList theme title transactions onSelect =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
        [ Typography.subsectionTitle theme title
        , if List.isEmpty transactions then
            Typography.mutedCaption theme "No transactions for this period"

          else
            Element.column
                [ Element.width Element.fill
                , Element.spacing Tokens.space4
                ]
                (List.map
                    (\transaction ->
                        Input.button
                            (Ui.rowSurfaceAttrs theme False ++ [ Element.width Element.fill ])
                            { onPress = Just (onSelect transaction)
                            , label =
                                Element.row
                                    [ Element.width Element.fill
                                    , Element.spacing Tokens.space14
                                    , Element.paddingXY Tokens.space4 Tokens.space10
                                    ]
                                    [ Element.paragraph
                                        [ Element.width Element.fill
                                        , Typography.minWidthZero
                                        , Typography.chartLabel
                                        ]
                                        [ Element.text transaction.description ]
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
        , columns : Int
        , legendState : LegendState
        , onLegendToggle : msg
        , highlightedCategory : Maybe Int
        , onCategoryHighlight : Int -> msg
        }
    -> Element msg
activityYear theme config =
    Ui.cardWith [ Element.width (Element.fillPortion Tokens.layoutPortionSingle), Element.alignTop ]
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
                        ActivityCalendar.yearBuckets response.period response.buckets

                    slots =
                        displayBuckets
                            |> List.map Just
                            |> padToMultiple config.columns Nothing
                in
                Element.column [ Element.width Element.fill, Element.spacing Tokens.space12 ]
                    [ activityLegend theme
                        config.tone
                        categories
                        config.legendState
                        config.onLegendToggle
                        config.highlightedCategory
                        config.onCategoryHighlight
                    , Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ]
                        (chunks config.columns slots
                            |> List.map
                                (\row ->
                                    Element.row
                                        [ Element.width Element.fill
                                        , Element.spacing Tokens.space16
                                        ]
                                        (List.map
                                            (\maybeBucket ->
                                                case maybeBucket of
                                                    Just bucket ->
                                                        Element.el
                                                            [ Element.width
                                                                (Element.fillPortion
                                                                    Tokens.layoutPortionSingle
                                                                )
                                                            , Element.paddingXY
                                                                Tokens.space0
                                                                Tokens.space8
                                                            ]
                                                            (activityMonthRing theme
                                                                config.commodity
                                                                config.tone
                                                                categories
                                                                config.highlightedCategory
                                                                (Maybe.map .key selected
                                                                    == Just bucket.key
                                                                )
                                                                bucket
                                                                config.onSelect
                                                            )

                                                    Nothing ->
                                                        Element.el
                                                            [ Element.width
                                                                (Element.fillPortion
                                                                    Tokens.layoutPortionSingle
                                                                )
                                                            , Element.paddingXY
                                                                Tokens.space0
                                                                Tokens.space8
                                                            ]
                                                            Element.none
                                            )
                                            row
                                        )
                                )
                        )
                    , case selected of
                        Nothing ->
                            Element.el
                                [ Element.width Element.fill
                                , Element.paddingXY Tokens.space0 Tokens.space12
                                ]
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
    case directionSpecsFor theme tone of
        Just specs ->
            activityDirectionYearLegend theme commodity specs bucket

        Nothing ->
            activityCategoryYearLegend theme commodity tone bucket


activityDirectionYearLegend : Theme -> String -> List DirectionSpec -> ActivityBucket -> Element msg
activityDirectionYearLegend theme commodity specs bucket =
    let
        section ( spec, amount ) =
            let
                base =
                    scaleColor spec.scale 0

                accounts =
                    categoriesForDirection spec.direction bucket
                        |> List.sortBy (.quantity >> decimalFloat >> abs >> negate)
            in
            Element.column [ Element.width Element.fill, Element.spacing Tokens.space4 ]
                (Element.row [ Element.width Element.fill, Element.spacing Tokens.space8 ]
                    [ Ui.accentDot base
                    , Element.el [ Element.width Element.fill, Font.semiBold ]
                        (Element.text spec.label)
                    , Element.el [ Font.color base, Font.semiBold, Typography.financeNumber ]
                        (Element.text (money commodity amount))
                    ]
                    :: List.map accountRow accounts
                )

        accountRow item =
            Element.row
                [ Element.width Element.fill
                , Element.spacing Tokens.space8
                , Element.paddingEach
                    { top = Tokens.space0
                    , bottom = Tokens.space0
                    , left = Tokens.space18
                    , right = Tokens.space0
                    }
                ]
                [ Element.el [ Element.width Element.fill, Typography.minWidthZero ]
                    (Typography.mutedCaption theme (categoryLabel item.account))
                , Element.el [ Font.color theme.textMuted, Typography.financeNumber ]
                    (Element.text (money commodity item.quantity))
                ]
    in
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space12 ]
        (Element.el
            [ Element.paddingXY Tokens.space0 Tokens.space8 ]
            (Typography.mutedBody theme (periodName bucket.key))
            :: List.map section (presentDirections specs bucket)
        )


activityCategoryYearLegend : Theme -> String -> ActivityTone -> ActivityBucket -> Element msg
activityCategoryYearLegend theme commodity tone bucket =
    let
        total =
            bucket.categories
                |> List.map (.quantity >> decimalFloat >> abs)
                |> List.sum

        items =
            bucket.categories
                |> List.sortBy (.quantity >> decimalFloat >> abs >> negate)

        totalColor =
            ringAccent theme tone bucket
    in
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
        [ Element.row
            [ Element.width Element.fill
            , Element.spacing Tokens.space8
            , Element.paddingXY Tokens.space0 Tokens.space8
            ]
            [ Typography.mutedBody theme (periodName bucket.key ++ " · ")
            , Element.el [ Font.color totalColor, Font.semiBold, Typography.financeNumber ]
                (Element.text (money commodity bucket.total))
            ]
        , Element.column [ Element.width Element.fill, Element.spacing Tokens.space4 ]
            (List.indexedMap
                (\index item ->
                    let
                        share =
                            if total <= 0 then
                                0

                            else
                                (decimalFloat item.quantity / total) * 100
                    in
                    Element.row [ Element.width Element.fill, Element.spacing Tokens.space8 ]
                        [ Icon.view
                            { icon =
                                AccountPresentation.iconForAccount
                                    item.accountType
                                    item.account
                            , size = Tokens.iconSizeSmallGlyph
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
                            Input.button
                                [ Element.width Element.fill
                                , Ui.touchTargetHeight
                                , Element.paddingXY Tokens.space0 Tokens.space10
                                ]
                                { onPress = Just (onSelect transaction)
                                , label =
                                    Element.row
                                        [ Element.width Element.fill
                                        , Element.spacing Tokens.space12
                                        ]
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
        Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
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


type alias DirectionSpec =
    { direction : ActivityDirection
    , label : String
    , scale : List Element.Color
    }


directionSpecsFor : Theme -> ActivityTone -> Maybe (List DirectionSpec)
directionSpecsFor theme tone =
    case tone of
        AssetTone ->
            Just
                [ { direction = Increase, label = "Acquire", scale = theme.acquireScale }
                , { direction = Reallocate, label = "Rebalance", scale = theme.rebalanceScale }
                , { direction = Decrease, label = "Dispose", scale = theme.disposeScale }
                ]

        LiabilityTone ->
            Just
                [ { direction = Increase, label = "Borrow", scale = theme.disposeScale }
                , { direction = Decrease, label = "Pay-off", scale = theme.acquireScale }
                ]

        _ ->
            Nothing


categoriesForDirection : ActivityDirection -> ActivityBucket -> List BreakdownItem
categoriesForDirection direction bucket =
    bucket.directions
        |> List.filter (.direction >> (==) direction)
        |> List.head
        |> Maybe.map .categories
        |> Maybe.withDefault []


scaleColor : List Element.Color -> Int -> Element.Color
scaleColor scale index =
    case scale of
        [] ->
            Theme.transparentColor

        _ ->
            scale
                |> List.drop (modBy (List.length scale) index)
                |> List.head
                |> Maybe.withDefault Theme.transparentColor


directionSegments : List DirectionSpec -> Maybe Int -> ActivityBucket -> List Ring.Segment
directionSegments specs highlighted bucket =
    specs
        |> List.indexedMap
            (\specIndex spec ->
                categoriesForDirection spec.direction bucket
                    |> List.indexedMap
                        (\accountIndex item ->
                            let
                                baseColor =
                                    scaleColor spec.scale accountIndex
                            in
                            { color =
                                if highlighted == Nothing || highlighted == Just specIndex then
                                    baseColor

                                else
                                    withAlpha 0.14 baseColor
                            , value = abs (decimalFloat item.quantity)
                            }
                        )
            )
        |> List.concat


ringAccent : Theme -> ActivityTone -> ActivityBucket -> Element.Color
ringAccent theme tone bucket =
    case directionSpecsFor theme tone of
        Nothing ->
            activityColor theme tone

        Just specs ->
            specs
                |> List.filterMap
                    (\spec ->
                        case categoriesForDirection spec.direction bucket of
                            [] ->
                                Nothing

                            items ->
                                Just
                                    ( List.sum (List.map (.quantity >> decimalFloat >> abs) items)
                                    , scaleColor spec.scale 0
                                    )
                    )
                |> List.sortBy (Tuple.first >> negate)
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault (activityColor theme tone)


directionAmount : ActivityDirection -> ActivityBucket -> Decimal
directionAmount direction bucket =
    case direction of
        Increase ->
            bucket.increase

        Reallocate ->
            bucket.reallocated

        Decrease ->
            bucket.decrease


presentDirections : List DirectionSpec -> ActivityBucket -> List ( DirectionSpec, Decimal )
presentDirections specs bucket =
    specs
        |> List.filterMap
            (\spec ->
                let
                    amount =
                        directionAmount spec.direction bucket
                in
                if decimalFloat amount > 0 then
                    Just ( spec, amount )

                else
                    Nothing
            )


monthRingCenter : Theme -> ActivityTone -> String -> Bool -> ActivityBucket -> List (Html msg)
monthRingCenter theme tone commodity isEmpty bucket =
    case directionSpecsFor theme tone of
        Just specs ->
            case presentDirections specs bucket of
                [] ->
                    [ Chart.ringValueText theme.textMuted (money commodity bucket.total) ]

                present ->
                    List.map (directionValueText commodity) present

        Nothing ->
            [ Chart.ringValueText
                (if isEmpty then
                    theme.textMuted

                 else
                    ringAccent theme tone bucket
                )
                (money commodity bucket.total)
            ]


directionValueText : String -> ( DirectionSpec, Decimal ) -> Html msg
directionValueText commodity ( spec, amount ) =
    Chart.ringValueText (scaleColor spec.scale 0) (money commodity amount)


activityLegend :
    Theme
    -> ActivityTone
    -> List BreakdownItem
    -> LegendState
    -> msg
    -> Maybe Int
    -> (Int -> msg)
    -> Element msg
activityLegend theme tone categories legendState onToggle highlighted onHighlight =
    case directionSpecsFor theme tone of
        Just specs ->
            activityDirectionLegend theme specs highlighted onHighlight

        Nothing ->
            activityCategoryLegend
                { theme = theme
                , tone = tone
                , categories = categories
                , legendState = legendState
                , onToggle = onToggle
                , highlightedCategory = highlighted
                , onCategoryHighlight = onHighlight
                }


activityDirectionLegend : Theme -> List DirectionSpec -> Maybe Int -> (Int -> msg) -> Element msg
activityDirectionLegend theme specs highlighted onHighlight =
    Element.wrappedRow [ Element.width Element.fill, Element.spacing Tokens.space10 ]
        (List.indexedMap
            (\index spec ->
                Ui.legendButton theme
                    { accent = scaleColor spec.scale 0
                    , selected = highlighted == Just index
                    , onPress = onHighlight index
                    , label =
                        Element.row [ Element.spacing Tokens.space5 ]
                            [ Ui.accentDot (scaleColor spec.scale 0)
                            , Typography.mutedCaption theme spec.label
                            ]
                    }
            )
            specs
        )


money : String -> Decimal -> String
money commodity amount =
    Formatting.formatAmountDisplay
        (Formatting.commodityDisplaySymbol commodity)
        (decimalFloat amount)


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
