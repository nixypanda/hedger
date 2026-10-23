module Ui.Skeleton exposing
    ( chartCard
    , chipRow
    , inputBar
    , listRow
    , summaryCard
    )

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Ui.Common exposing (uiClass)
import Ui.DesignTokens
    exposing
        ( radiusCard
        , radiusControl
        , radiusPill
        , radiusSmall
        , space12
        , space20
        , space4
        , space8
        )
import Ui.Theme exposing (Theme)


summaryCard : Theme -> Element msg
summaryCard theme =
    cardBase theme
        [ Element.row [ Element.width Element.fill, Element.spacing space12 ]
            [ Element.el
                [ Element.width (Element.px 42)
                , Element.height (Element.px 42)
                , Background.color theme.inputFill
                , Border.rounded radiusControl
                ]
                Element.none
            , Element.el
                [ Element.width (Element.px 126)
                , Element.height (Element.px 48)
                , Background.color theme.inputFill
                , Border.rounded radiusSmall
                ]
                Element.none
            ]
        , barNarrow theme
        , barWide theme
        , barNarrow theme
        ]


chartCard : Theme -> Element msg
chartCard theme =
    cardBase theme
        [ barHalf theme
        , Element.el
            [ Element.width Element.fill
            , Element.height (Element.px 200)
            , Background.color theme.inputFill
            , Border.rounded radiusSmall
            ]
            Element.none
        , Element.row [ Element.width Element.fill, Element.spacing space8 ]
            [ Element.el
                [ Element.width (Element.px 8)
                , Element.height (Element.px 8)
                , Background.color theme.inputFill
                , Border.rounded radiusPill
                ]
                Element.none
            , Element.el
                [ Element.width (Element.px 80)
                , Element.height (Element.px 12)
                , Background.color theme.inputFill
                , Border.rounded radiusSmall
                ]
                Element.none
            , Element.el
                [ Element.width (Element.px 8)
                , Element.height (Element.px 8)
                , Background.color theme.inputFill
                , Border.rounded radiusPill
                ]
                Element.none
            , Element.el
                [ Element.width (Element.px 60)
                , Element.height (Element.px 12)
                , Background.color theme.inputFill
                , Border.rounded radiusSmall
                ]
                Element.none
            ]
        ]


listRow : Theme -> Element msg
listRow theme =
    Element.row
        [ Element.width Element.fill
        , Element.spacing space12
        , Element.paddingXY 12 12
        ]
        [ Element.el
            [ Element.width (Element.px 18)
            , Element.height (Element.px 18)
            , Background.color theme.inputFill
            , Border.rounded radiusSmall
            ]
            Element.none
        , Element.column [ Element.width Element.fill, Element.spacing space4 ]
            [ barHalf theme
            , barNarrow theme
            ]
        , Element.el
            [ Element.width (Element.px 80)
            , Element.height (Element.px 16)
            , Background.color theme.inputFill
            , Border.rounded radiusSmall
            ]
            Element.none
        ]


inputBar : Theme -> Element msg
inputBar theme =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px 40)
        , Background.color theme.inputFill
        , Border.rounded radiusSmall
        , uiClass "ui-skeleton"
        ]
        Element.none


chipRow : Theme -> Element msg
chipRow theme =
    Element.row [ Element.width Element.fill, Element.spacing space8 ]
        [ chipSkeleton theme
        , chipSkeleton theme
        , chipSkeleton theme
        , chipSkeleton theme
        ]



-- Internal helpers


cardBase : Theme -> List (Element msg) -> Element msg
cardBase theme children =
    Element.column
        [ Element.width Element.fill
        , Background.color theme.cardBg
        , Border.rounded radiusCard
        , Element.padding space20
        , Element.spacing space12
        , Border.width 1
        , Border.color theme.divider
        , uiClass "ui-card-shadow ui-surface"
        ]
        children


barWide : Theme -> Element msg
barWide theme =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px 16)
        , Background.color theme.inputFill
        , Border.rounded radiusSmall
        , uiClass "ui-skeleton"
        ]
        Element.none


barHalf : Theme -> Element msg
barHalf theme =
    Element.el
        [ Element.width (Element.fillPortion 1)
        , Element.height (Element.px 16)
        , Background.color theme.inputFill
        , Border.rounded radiusSmall
        , uiClass "ui-skeleton"
        ]
        Element.none


barNarrow : Theme -> Element msg
barNarrow theme =
    Element.el
        [ Element.width (Element.fillPortion 1)
        , Element.height (Element.px 12)
        , Background.color theme.inputFill
        , Border.rounded radiusSmall
        , uiClass "ui-skeleton"
        ]
        Element.none


chipSkeleton : Theme -> Element msg
chipSkeleton theme =
    Element.el
        [ Element.width (Element.px 80)
        , Element.height (Element.px 28)
        , Background.color theme.inputFill
        , Border.rounded radiusSmall
        , uiClass "ui-skeleton"
        ]
        Element.none
