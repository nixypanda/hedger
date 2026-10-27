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
import Html.Attributes as Attr
import Ui.Button as Button
import Ui.Common exposing (minWidthZero, uiClass)
import Ui.DesignTokens
    exposing
        ( minimumTouchTarget
        , periodFiscalYearLabelWidth
        , periodMonthLabelWidth
        , space12
        , space24
        , space32
        , space4
        , space8
        )
import Ui.Icon as Icon
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import Ui.Viewport as Viewport exposing (Viewport)


layout : Theme -> Element msg -> Element msg
layout theme content =
    Element.column
        (Typography.body
            ++ [ Element.width Element.fill
               , Element.height Element.fill
               , Background.color theme.pageBg
               , Typography.fontFamily
               , Font.color theme.textPrimary
               , uiClass "ui-theme-root"
               ]
        )
        [ content ]


pageContainer : List (Element msg) -> Element msg
pageContainer children =
    Element.column
        [ Element.centerX
        , Element.width (Element.fill |> Element.maximum 1280)
        , Element.spacing space24
        , Element.padding Viewport.contentPadding
        ]
        children


type alias Header msg =
    { leading : Maybe (Element msg)
    , title : Element msg
    , subtitle : Maybe String
    , actions : List (Element msg)
    }


scaffold :
    Viewport
    -> Theme
    -> { header : Header msg, body : List (Element msg) }
    -> Element msg
scaffold viewport theme config =
    Element.column
        [ Element.width Element.fill ]
        [ pageHeaderBand theme (pageHeader viewport theme config.header)
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
            , Element.paddingEach
                { top = space32
                , right = Viewport.contentPadding
                , bottom = space24
                , left = Viewport.contentPadding
                }
            ]
            content
        )


pageHeader :
    Viewport
    -> Theme
    -> Header msg
    -> Element msg
pageHeader viewport theme config =
    if Viewport.isCompact (Viewport.contentLayout viewport) then
        compactPageHeader theme config

    else
        widePageHeader theme config


compactPageHeader : Theme -> Header msg -> Element msg
compactPageHeader theme config =
    Element.column
        [ Element.width Element.fill
        , Element.spacing space12
        , minWidthZero
        ]
        [ Element.row
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
                (headerTitles theme config)
            ]
        , if List.isEmpty config.actions then
            Element.none

          else
            Element.wrappedRow
                [ Element.width Element.fill
                , Element.spacing space8
                ]
                config.actions
        ]


widePageHeader : Theme -> Header msg -> Element msg
widePageHeader theme config =
    Element.wrappedRow
        [ Element.width Element.fill
        , Element.spacing space12
        , minWidthZero
        ]
        [ Maybe.withDefault Element.none config.leading
        , Element.column
            [ Element.width Element.fill
            , Element.htmlAttribute
                (Attr.style
                    "flex-basis"
                    (String.fromInt pageHeaderTitleMinWidth ++ "px")
                )
            , Element.spacing space8
            , minWidthZero
            ]
            (headerTitles theme config)
        , if List.isEmpty config.actions then
            Element.none

          else
            Element.row
                [ Element.width Element.shrink
                , Element.spacing space8
                ]
                config.actions
        ]


pageHeaderTitleMinWidth : Int
pageHeaderTitleMinWidth =
    240


headerTitles : Theme -> Header msg -> List (Element msg)
headerTitles theme config =
    config.title
        :: (case config.subtitle of
                Just subtitle ->
                    [ Typography.mutedParagraph theme subtitle ]

                Nothing ->
                    []
           )


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
                (Typography.labelSmall ++ [ Element.centerX, Element.centerY ])
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
