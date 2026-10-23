module Ui.DesignTokens exposing
    ( analyticsChartHeight
    , analyticsRingSize
    , borderHairline
    , compactControlVisualHeight
    , iconSizeCaption
    , iconSizeCompact
    , iconSizeDetail
    , iconSizeNavigation
    , iconSizeSmall
    , iconSizeSmallGlyph
    , iconSizeTiny
    , iconSizeTinyGlyph
    , investmentNavigatorWidth
    , investmentProgressWidth
    , layoutPortionDouble
    , layoutPortionSingle
    , layoutPortionTriple
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


analyticsChartHeight : Int
analyticsChartHeight =
    200


analyticsRingSize : Int
analyticsRingSize =
    42


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
