module Ui.Skeleton exposing
    ( activityCard
    , activityLegend
    , activityRing
    , allocationCard
    , block
    , categoryCard
    , fillBlock
    , historyCard
    , kpiCard
    , listRow
    , metricCard
    , timelineCard
    , titledChartCard
    )

{-| Low-depth loading placeholders that mirror the outer chrome and the major
regions of real cards. Use these instead of rebuilding a page's card layout.
-}

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Html.Attributes as Attr
import Ui.Common exposing (uiClass)
import Ui.DesignTokens as Tokens
import Ui.Surface exposing (DonutLayout(..))
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import Ui.Viewport as Viewport


{-| A card with the same padding, border, radius, shadow, and spacing as
`Ui.Surface.card`, fixed to the supplied height.
-}
card : Theme -> Int -> List (Element msg) -> Element msg
card theme height children =
    cardWith theme [ Element.height (Element.px height) ] children


cardWith : Theme -> List (Attribute msg) -> List (Element msg) -> Element msg
cardWith theme attributes children =
    Element.column
        (attributes
            ++ [ Element.width Element.fill
               , Background.color theme.cardBg
               , Border.rounded Tokens.radiusCard
               , Element.padding Tokens.space20
               , Element.spacing Tokens.space12
               , Border.width Tokens.borderHairline
               , Border.color theme.divider
               , uiClass "ui-card-shadow ui-surface"
               ]
        )
        children


kpiCard : Theme -> Element msg
kpiCard theme =
    card theme
        Tokens.kpiCardHeight
        [ Element.row [ Element.width Element.fill, Element.spacing Tokens.space12 ]
            [ block theme Tokens.kpiIconTileSize Tokens.kpiIconTileSize
            , block theme Tokens.kpiSparklineWidth Tokens.kpiSparklineHeight
            ]
        , block theme Tokens.skeletonLineLong Tokens.bodyTextHeight
        , block theme Tokens.skeletonLineLong Tokens.kpiValueHeight
        , block theme Tokens.skeletonLineShort Tokens.kpiComparisonHeight
        ]


metricCard : Theme -> Element msg
metricCard theme =
    card theme
        Tokens.metricCardHeight
        [ Element.row
            [ Element.width Element.fill
            , Element.spacing Tokens.space12
            , Element.alignTop
            ]
            [ block theme Tokens.compactControlVisualHeight Tokens.compactControlVisualHeight
            , Element.column [ Element.width Element.fill, Element.spacing Tokens.space4 ]
                [ block theme Tokens.skeletonLineShort 12
                , block theme Tokens.skeletonLineLong 32
                ]
            ]
        ]


{-| Analytics timeline card: title, chart, legend, and the selected-period
details area that the real timeline normally shows.
-}
timelineCard : Theme -> String -> Element msg
timelineCard theme title =
    card theme
        Tokens.analysisTimelineCardHeight
        [ Typography.cardTitle theme title
        , chart theme Tokens.chartHeightTimeline
        , legend theme
        , fillBlock theme timelineDetailsHeight
        ]


{-| Category breakdown card: title, donut region, and the category row region,
using the same stacked/side-by-side structure and fixed height as the real card.
-}
categoryCard :
    Theme
    -> DonutLayout
    -> Int
    -> String
    -> Element msg
categoryCard theme donutLayout cardHeight title =
    card theme
        cardHeight
        [ Typography.cardTitle theme title
        , categoryContent theme donutLayout cardHeight
        ]


categoryContent : Theme -> DonutLayout -> Int -> Element msg
categoryContent theme donutLayout cardHeight =
    let
        stacked =
            donutLayout == DonutStacked

        contentHeight =
            cardHeight
                - cardChromeHeight
                - Tokens.chartTitleHeight
                - Tokens.space12

        donut =
            block
                theme
                Tokens.analyticsDonutSize
                Tokens.analyticsDonutSize

        rowsHeight =
            if stacked then
                contentHeight - Tokens.analyticsDonutSize - Tokens.space16

            else
                contentHeight
    in
    if stacked then
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.spacing Tokens.space16
            , Element.alignTop
            ]
            [ Element.el [ Element.centerX ] donut, fillBlock theme rowsHeight ]

    else
        Element.row
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.spacing Tokens.space24
            , Element.alignTop
            ]
            [ donut, fillBlock theme rowsHeight ]


{-| Activity calendar card: title, calendar grid, legend, and details prompt.
-}
activityCard : Theme -> Int -> String -> Element msg
activityCard theme legendItems title =
    card theme
        Tokens.analysisActivityCardHeight
        [ Typography.cardTitle theme title
        , fillBlock theme activityCalendarHeight
        , activityLegend theme legendItems
        , fillBlock theme activityDetailsHeight
        ]


{-| Asset allocation card: title and allocation cells using the real grid's
column count for the current layout.
-}
allocationCard : Theme -> Viewport.ContentLayout -> Element msg
allocationCard theme layout =
    let
        ( columns, rows ) =
            case layout of
                Viewport.Compact ->
                    ( 1, 4 )

                Viewport.Medium ->
                    ( 2, 2 )

                Viewport.Wide ->
                    ( 3, 2 )
    in
    card theme
        (allocationCardHeight rows)
        [ Typography.cardTitle theme "Asset allocation"
        , Element.column
            [ Element.width Element.fill
            , Element.spacing Tokens.space12
            ]
            (List.repeat rows (allocationRow theme columns))
        ]


allocationCardHeight : Int -> Int
allocationCardHeight rows =
    2
        * Tokens.space20
        + 2
        * Tokens.borderHairline
        + Tokens.chartTitleHeight
        + Tokens.space12
        + rows
        * Tokens.investmentAllocationRowHeight
        + (rows - 1)
        * Tokens.space12


{-| Portfolio history card: header with chart-mode control, chart, selected
period details, and legend.
-}
historyCard : Theme -> Element msg
historyCard theme =
    card theme
        Tokens.investmentHistoryCardHeight
        [ Element.row [ Element.width Element.fill ]
            [ Typography.cardTitle theme "Portfolio history"
            , Element.el [ Element.alignRight ]
                (block theme Tokens.skeletonLineLong investmentControlHeight)
            ]
        , chart theme Tokens.chartHeightInvestment
        , fillBlock theme investmentDetailsHeight
        , legend theme
        ]


{-| Activity legend placeholder at the real legend's minimum touch height.
-}
activityLegend : Theme -> Int -> Element msg
activityLegend theme legendItems =
    Element.wrappedRow
        [ Element.width Element.fill
        , Element.height (Element.px Tokens.minimumTouchTarget)
        , Element.spacing Tokens.space8
        ]
        (List.repeat (max 1 legendItems) (legendChip theme))


{-| A circular placeholder that follows the real yearly activity ring's
responsive size constraints.
-}
activityRing : Theme -> Element msg
activityRing theme =
    Element.el
        [ Element.width Element.fill
        , Element.htmlAttribute
            (Attr.style "min-width" (String.fromInt Tokens.activityYearRingMinSize ++ "px"))
        , Element.htmlAttribute
            (Attr.style "max-width" (String.fromInt Tokens.activityYearRingMaxSize ++ "px"))
        , Element.htmlAttribute (Attr.style "aspect-ratio" "1")
        , Background.color theme.inputFill
        , Border.rounded Tokens.radiusPill
        , uiClass "ui-skeleton"
        ]
        Element.none


{-| Chart placeholder with a real, static card title instead of a title bar.
-}
titledChartCard :
    Theme
    -> { chartHeight : Int, title : String, showLegend : Bool }
    -> Element msg
titledChartCard theme config =
    let
        children =
            [ Typography.cardTitle theme config.title
            , chart theme config.chartHeight
            ]
                ++ (if config.showLegend then
                        [ legend theme ]

                    else
                        []
                   )

        height =
            2
                * Tokens.space20
                + 2
                * Tokens.borderHairline
                + Tokens.chartTitleHeight
                + config.chartHeight
                + (if config.showLegend then
                    Tokens.chartLegendHeight

                   else
                    0
                  )
                + Tokens.space12
                * max 0 (List.length children - 1)
    in
    card theme height children


listRow : Theme -> Element msg
listRow theme =
    Element.row
        [ Element.width Element.fill
        , Element.spacing Tokens.space12
        , Element.paddingXY Tokens.space12 Tokens.space12
        ]
        [ block theme Tokens.iconSizeCaption Tokens.iconSizeCaption
        , Element.column [ Element.width Element.fill, Element.spacing Tokens.space4 ]
            [ barHalf theme
            , barNarrow theme
            ]
        , block theme Tokens.skeletonLineShort 16
        ]



-- Internal helpers


cardChromeHeight : Int
cardChromeHeight =
    2 * Tokens.space20 + 2 * Tokens.borderHairline


timelineDetailsHeight : Int
timelineDetailsHeight =
    Tokens.analysisTimelineCardHeight
        - cardChromeHeight
        - Tokens.chartTitleHeight
        - Tokens.chartHeightTimeline
        - Tokens.chartLegendHeight
        - 3
        * Tokens.space12


activityDetailsHeight : Int
activityDetailsHeight =
    Tokens.iconSizeTiny + 2 * Tokens.space12


activityCalendarHeight : Int
activityCalendarHeight =
    Tokens.analysisActivityCardHeight
        - cardChromeHeight
        - Tokens.chartTitleHeight
        - Tokens.minimumTouchTarget
        - activityDetailsHeight
        - 3
        * Tokens.space12


investmentControlHeight : Int
investmentControlHeight =
    Tokens.minimumTouchTarget + 2 * Tokens.space4 + 2 * Tokens.borderHairline


investmentDetailsHeight : Int
investmentDetailsHeight =
    Tokens.investmentHistoryCardHeight
        - cardChromeHeight
        - investmentControlHeight
        - Tokens.chartHeightInvestment
        - Tokens.chartLegendHeight
        - 3
        * Tokens.space12


chart : Theme -> Int -> Element msg
chart theme height =
    fillBlock theme height


fillBlock : Theme -> Int -> Element msg
fillBlock theme height =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px height)
        , Background.color theme.inputFill
        , Border.rounded Tokens.radiusSmall
        , uiClass "ui-skeleton"
        ]
        Element.none


allocationRow : Theme -> Int -> Element msg
allocationRow theme columns =
    Element.row
        [ Element.width Element.fill
        , Element.spacing Tokens.space12
        ]
        (List.repeat columns (allocationCell theme))


allocationCell : Theme -> Element msg
allocationCell theme =
    Element.el
        [ Element.width (Element.fillPortion Tokens.layoutPortionSingle)
        , Element.height (Element.px Tokens.investmentAllocationRowHeight)
        , Background.color theme.inputFill
        , Border.rounded Tokens.radiusControl
        , uiClass "ui-skeleton"
        ]
        Element.none


legendChip : Theme -> Element msg
legendChip theme =
    Element.el
        [ Element.width (Element.px Tokens.skeletonLineTiny)
        , Element.height (Element.px Tokens.minimumTouchTarget)
        , Background.color theme.inputFill
        , Border.rounded Tokens.radiusPill
        , uiClass "ui-skeleton"
        ]
        Element.none


legend : Theme -> Element msg
legend theme =
    Element.row
        [ Element.width Element.fill
        , Element.height (Element.px Tokens.chartLegendHeight)
        , Element.spacing Tokens.space16
        ]
        [ legendItem theme Tokens.skeletonLineShort
        , legendItem theme Tokens.skeletonLineTiny
        ]


legendItem : Theme -> Int -> Element msg
legendItem theme width =
    Element.row
        [ Element.spacing Tokens.space8
        , Element.centerY
        ]
        [ Element.el
            [ Element.width (Element.px Tokens.space8)
            , Element.height (Element.px Tokens.space8)
            , Background.color theme.inputFill
            , Border.rounded Tokens.radiusPill
            ]
            Element.none
        , block theme width 12
        ]


block : Theme -> Int -> Int -> Element msg
block theme width height =
    Element.el
        [ Element.width (Element.px width)
        , Element.height (Element.px height)
        , Background.color theme.inputFill
        , Border.rounded Tokens.radiusSmall
        , uiClass "ui-skeleton"
        ]
        Element.none


barHalf : Theme -> Element msg
barHalf theme =
    Element.el
        [ Element.width (Element.fillPortion Tokens.layoutPortionSingle)
        , Element.height (Element.px 16)
        , Background.color theme.inputFill
        , Border.rounded Tokens.radiusSmall
        , uiClass "ui-skeleton"
        ]
        Element.none


barNarrow : Theme -> Element msg
barNarrow theme =
    Element.el
        [ Element.width (Element.fillPortion Tokens.layoutPortionSingle)
        , Element.height (Element.px 12)
        , Background.color theme.inputFill
        , Border.rounded Tokens.radiusSmall
        , uiClass "ui-skeleton"
        ]
        Element.none
