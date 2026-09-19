module Ui.Skeleton exposing
    ( bar
    , chartCard
    , chipRow
    , datePair
    , inputBar
    , kpiCard
    , listRow
    , loading
    , metricCard
    )

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Ui.DesignTokens
    exposing
        ( radiusCard
        , radiusSmall
        , space12
        , space20
        , space4
        , space8
        )
import Ui.Theme exposing (Theme)
import Ui.Typography exposing (uiClass)


loading : Theme -> Element msg
loading theme =
    Element.column [ Element.width Element.fill, Element.spacing space12 ]
        [ bar theme
        , bar theme
        , bar theme
        ]


bar : Theme -> Element msg
bar theme =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px 16)
        , Background.color theme.inputFill
        , Border.rounded radiusSmall
        , uiClass "ui-skeleton"
        ]
        Element.none


kpiCard : Theme -> Element msg
kpiCard theme =
    cardBase theme
        [ Element.row [ Element.width Element.fill, Element.spacing space12 ]
            [ Element.el
                [ Element.width (Element.px 42)
                , Element.height (Element.px 42)
                , Background.color theme.inputFill
                , Border.rounded 10
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


metricCard : Theme -> Element msg
metricCard theme =
    cardBase theme
        [ Element.row [ Element.width Element.fill ]
            [ Element.el
                [ Element.width (Element.px 22)
                , Element.height (Element.px 22)
                , Background.color theme.inputFill
                , Border.rounded 4
                ]
                Element.none
            , Element.el [ Element.width Element.fill ] Element.none
            , Element.el
                [ Element.width (Element.px 60)
                , Element.height (Element.px 12)
                , Background.color theme.inputFill
                , Border.rounded radiusSmall
                ]
                Element.none
            ]
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
                , Border.rounded 4
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
                , Border.rounded 4
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
        , Element.paddingXY 12 10
        ]
        [ Element.el
            [ Element.width (Element.px 18)
            , Element.height (Element.px 18)
            , Background.color theme.inputFill
            , Border.rounded 4
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


datePair : Theme -> Element msg
datePair theme =
    Element.row [ Element.width Element.fill, Element.spacing space12 ]
        [ Element.column
            [ Element.width (Element.fill |> Element.maximum 220)
            , Element.spacing space4
            ]
            [ barNarrow theme
            , inputBar theme
            ]
        , Element.column
            [ Element.width (Element.fill |> Element.maximum 220)
            , Element.spacing space4
            ]
            [ barNarrow theme
            , inputBar theme
            ]
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
