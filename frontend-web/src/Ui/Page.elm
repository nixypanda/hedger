module Ui.Page exposing
    ( Header
    , PeriodLabelKind(..)
    , layout
    , pageContainer
    , pageHeader
    , pageHeaderBand
    , periodNavigation
    , scaffold
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
        , space12
        , space16
        , space4
        , space8
        )
import Ui.Icon as Icon
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


type alias Header msg =
    { leading : Maybe (Element msg)
    , title : Element msg
    , subtitle : Maybe String
    , actions : List (Element msg)
    }


scaffold :
    Theme
    -> { header : Header msg, body : List (Element msg) }
    -> Element msg
scaffold theme config =
    Element.column
        [ Element.width Element.fill ]
        [ pageHeaderBand theme (pageHeader theme config.header)
        , pageContainer config.body
        ]


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
    -> Header msg
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
