module Ui exposing
    ( PeriodLabelKind(..)
    , emptyState
    , errorBanner
    , layout
    , loading
    , pageContainer
    , pageHeader
    , pageHeaderBand
    , periodNavigation
    )

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Ui.Button as Button
import Ui.Common exposing (minWidthZero, uiClass)
import Ui.DesignTokens
    exposing
        ( minimumTouchTarget
        , periodFiscalYearLabelWidth
        , periodMonthLabelWidth
        , radiusControl
        , space12
        , space16
        , space4
        , space8
        )
import Ui.Icon as Icon
import Ui.Surface as Surface
import Ui.Theme exposing (Theme)
import Ui.Typography exposing (body, mutedParagraph)
import Ui.Viewport


layout : Theme -> Element msg -> Element msg
layout theme content =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color theme.pageBg
        , Font.family [ Font.typeface "Inter", Font.sansSerif ]
        , Font.color theme.textPrimary
        , body
        , uiClass "ui-theme-root"
        ]
        [ content ]


pageContainer : List (Element msg) -> Element msg
pageContainer children =
    Element.column
        [ Element.centerX
        , Element.width (Element.fill |> Element.maximum 1280)
        , Element.spacing space16
        , Element.padding Ui.Viewport.contentPadding
        ]
        children


pageHeaderBand : Theme -> Element msg -> Element msg
pageHeaderBand theme content =
    Element.el
        [ Element.width Element.fill
        , Background.color theme.pageBg
        , Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
        , Border.color theme.pageBg
        ]
        (Element.el
            [ Element.centerX
            , Element.width (Element.fill |> Element.maximum 1280)
            , Element.padding Ui.Viewport.contentPadding
            ]
            content
        )


pageHeader :
    Theme
    ->
        { leading : Maybe (Element msg)
        , title : Element msg
        , subtitle : Maybe String
        , actions : List (Element msg)
        }
    -> Element msg
pageHeader theme config =
    Element.wrappedRow
        [ Element.width Element.fill
        , Element.spacing space12
        , minWidthZero
        ]
        [ Maybe.withDefault Element.none config.leading
        , Element.column
            [ Element.width Element.fill
            , Element.spacing space8
            , minWidthZero
            ]
            (config.title
                :: (case config.subtitle of
                        Just subtitle ->
                            [ mutedParagraph theme subtitle ]

                        Nothing ->
                            []
                   )
            )
        , if List.isEmpty config.actions then
            Element.none

          else
            Element.row [ Element.spacing space8 ] config.actions
        ]


periodNavigation :
    Theme
    ->
        { label : String
        , labelKind : PeriodLabelKind
        , onPrevious : msg
        , onNext : msg
        }
    -> Element msg
periodNavigation theme config =
    Element.row [ Element.spacing space4, Element.alignRight ]
        [ Button.iconButton theme
            { icon = Icon.ChevronLeft
            , title = "Previous period"
            , onPress = config.onPrevious
            }
        , Element.el
            [ Element.width (Element.px (periodLabelWidth config.labelKind))
            , Element.height (Element.px minimumTouchTarget)
            , Element.centerY
            ]
            (Element.el
                [ Element.centerX, Element.centerY, body, Font.medium ]
                (Element.text config.label)
            )
        , Button.iconButton theme
            { icon = Icon.ChevronRight
            , title = "Next period"
            , onPress = config.onNext
            }
        ]


type PeriodLabelKind
    = MonthPeriodLabel
    | FiscalYearPeriodLabel


periodLabelWidth : PeriodLabelKind -> Int
periodLabelWidth labelKind =
    case labelKind of
        MonthPeriodLabel ->
            periodMonthLabelWidth

        FiscalYearPeriodLabel ->
            periodFiscalYearLabelWidth


loading : Theme -> String -> Element msg
loading theme message =
    Element.row
        [ Element.spacing space8
        , Font.color theme.textMuted
        ]
        [ Icon.view { icon = Icon.LoaderCircle, size = 18, color = theme.textMuted }
        , Element.text message
        ]


emptyState : Theme -> String -> Element msg
emptyState theme message =
    Surface.card theme
        [ Element.paragraph
            [ Font.color theme.textMuted
            , Font.center
            , Element.padding space8
            ]
            [ Element.text message ]
        ]


errorBanner : Theme -> String -> Element msg
errorBanner theme message =
    Element.row
        [ Element.width Element.fill
        , Element.spacing space8
        , Element.padding space12
        , Background.color theme.dangerBg
        , Font.color theme.dangerFg
        , Border.rounded radiusControl
        ]
        [ Icon.view { icon = Icon.AlertTriangle, size = 18, color = theme.dangerFg }
        , Element.paragraph [ Element.width Element.fill ] [ Element.text message ]
        ]
