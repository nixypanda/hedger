module Ui.DesignTokens exposing
    ( activityYearRingMaxSize
    , activityYearRingMinSize
    , analysisActivityCardHeight
    , analysisCategoryCardCompactHeight
    , analysisCategoryCardHeight
    , analysisTimelineCardHeight
    , analyticsDonutSize
    , bodyTextHeight
    , borderHairline
    , calendarCellHeight
    , chartHeightInvestment
    , chartHeightStandard
    , chartHeightTimeline
    , chartLegendHeight
    , chartTitleHeight
    , compactControlVisualHeight
    , iconSizeCaption
    , iconSizeCompact
    , iconSizeDetail
    , iconSizeNavigation
    , iconSizeSmall
    , iconSizeSmallGlyph
    , iconSizeTiny
    , iconSizeTinyGlyph
    , investmentAllocationCardCompactHeight
    , investmentAllocationCardHeight
    , investmentAllocationCardMediumHeight
    , investmentHistoryCardHeight
    , investmentNavigatorCompactHeight
    , investmentNavigatorWidth
    , investmentProgressWidth
    , kpiCardHeight
    , kpiComparisonHeight
    , kpiIconTileSize
    , kpiSparklineHeight
    , kpiSparklineWidth
    , kpiValueHeight
    , layoutPortionDouble
    , layoutPortionSingle
    , layoutPortionTriple
    , metricCardHeight
    , minimumTouchTarget
    , narrowContentWidth
    , navigationBarHeight
    , navigationBrandSize
    , periodFiscalYearLabelWidth
    , periodMonthLabelWidth
    , postingAccountWidth
    , radiusCard
    , radiusControl
    , radiusHairline
    , radiusPill
    , radiusSmall
    , registerAmountWidth
    , registerDateWidth
    , sidebarWidth
    , skeletonLineLong
    , skeletonLineShort
    , skeletonLineTiny
    , space0
    , space12
    , space16
    , space2
    , space20
    , space24
    , space32
    , space4
    , space8
    , splitPaneControlWidth
    , statusDotSize
    , statusIndicatorHeight
    , statusIndicatorWidth
    , subsectionTitleHeight
    , treeIndentWidth
    )


compactControlVisualHeight : Int
compactControlVisualHeight =
    36


minimumTouchTarget : Int
minimumTouchTarget =
    44


borderHairline : Int
borderHairline =
    1


analyticsDonutSize : Int
analyticsDonutSize =
    240


bodyTextHeight : Int
bodyTextHeight =
    14


activityYearRingMinSize : Int
activityYearRingMinSize =
    70


activityYearRingMaxSize : Int
activityYearRingMaxSize =
    110


analysisTimelineCardHeight : Int
analysisTimelineCardHeight =
    499


analysisCategoryCardHeight : Int
analysisCategoryCardHeight =
    357


analysisCategoryCardCompactHeight : Int
analysisCategoryCardCompactHeight =
    540


analysisActivityCardHeight : Int
analysisActivityCardHeight =
    623


calendarCellHeight : Int
calendarCellHeight =
    72


chartHeightStandard : Int
chartHeightStandard =
    250


chartHeightTimeline : Int
chartHeightTimeline =
    300


chartHeightInvestment : Int
chartHeightInvestment =
    260


chartLegendHeight : Int
chartLegendHeight =
    24


chartTitleHeight : Int
chartTitleHeight =
    21


kpiCardHeight : Int
kpiCardHeight =
    200


kpiIconTileSize : Int
kpiIconTileSize =
    42


kpiValueHeight : Int
kpiValueHeight =
    32


kpiComparisonHeight : Int
kpiComparisonHeight =
    12


kpiSparklineHeight : Int
kpiSparklineHeight =
    48


kpiSparklineWidth : Int
kpiSparklineWidth =
    126


metricCardHeight : Int
metricCardHeight =
    90


investmentAllocationCardHeight : Int
investmentAllocationCardHeight =
    243


investmentAllocationCardMediumHeight : Int
investmentAllocationCardMediumHeight =
    333


investmentAllocationCardCompactHeight : Int
investmentAllocationCardCompactHeight =
    609


investmentHistoryCardHeight : Int
investmentHistoryCardHeight =
    530


investmentNavigatorCompactHeight : Int
investmentNavigatorCompactHeight =
    355


statusIndicatorWidth : Int
statusIndicatorWidth =
    120


statusIndicatorHeight : Int
statusIndicatorHeight =
    44


statusDotSize : Int
statusDotSize =
    10


{-| Rendered height of `Ui.Typography.subsectionTitle` (20px at 1.25 leading).
-}
subsectionTitleHeight : Int
subsectionTitleHeight =
    25


layoutPortionSingle : Int
layoutPortionSingle =
    1


layoutPortionDouble : Int
layoutPortionDouble =
    2


layoutPortionTriple : Int
layoutPortionTriple =
    3


iconSizeTiny : Int
iconSizeTiny =
    12


iconSizeTinyGlyph : Int
iconSizeTinyGlyph =
    14


iconSizeSmallGlyph : Int
iconSizeSmallGlyph =
    16


iconSizeNavigation : Int
iconSizeNavigation =
    19


iconSizeDetail : Int
iconSizeDetail =
    22


iconSizeCaption : Int
iconSizeCaption =
    18


iconSizeCompact : Int
iconSizeCompact =
    32


iconSizeSmall : Int
iconSizeSmall =
    20


sidebarWidth : Int
sidebarWidth =
    232


navigationBarHeight : Int
navigationBarHeight =
    72


navigationBrandSize : Int
navigationBrandSize =
    34


narrowContentWidth : Int
narrowContentWidth =
    180


splitPaneControlWidth : Int
splitPaneControlWidth =
    220


postingAccountWidth : Int
postingAccountWidth =
    112


treeIndentWidth : Int
treeIndentWidth =
    20


investmentNavigatorWidth : Int
investmentNavigatorWidth =
    320


investmentProgressWidth : Int
investmentProgressWidth =
    3


registerDateWidth : Int
registerDateWidth =
    92


registerAmountWidth : Int
registerAmountWidth =
    150


skeletonLineTiny : Int
skeletonLineTiny =
    60


skeletonLineShort : Int
skeletonLineShort =
    80


skeletonLineLong : Int
skeletonLineLong =
    160


periodMonthLabelWidth : Int
periodMonthLabelWidth =
    84


periodFiscalYearLabelWidth : Int
periodFiscalYearLabelWidth =
    112



-- Spacing scale
--
-- The scale is intentionally coarse: use these steps for layout gaps and
-- padding. Fixed layout/component dimensions stay in the constants above.


space0 : Int
space0 =
    0


space2 : Int
space2 =
    2


space4 : Int
space4 =
    4


space8 : Int
space8 =
    8


space12 : Int
space12 =
    12


space16 : Int
space16 =
    16


space20 : Int
space20 =
    20


space24 : Int
space24 =
    24


space32 : Int
space32 =
    32



-- Border radius scale


radiusHairline : Int
radiusHairline =
    2


radiusSmall : Int
radiusSmall =
    8


radiusControl : Int
radiusControl =
    12


radiusCard : Int
radiusCard =
    20


radiusPill : Int
radiusPill =
    999
