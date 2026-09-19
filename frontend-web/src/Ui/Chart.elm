module Ui.Chart exposing
    ( Layout
    , activeBand
    , fixedAxis
    , gridLines
    , hitTarget
    , layout
    , marker
    , plotAttributes
    , ringDayLabel
    , ringMonthCenter
    , ringValueText
    , roundedBar
    , xAxisLabel
    )

import Element exposing (Color, Element)
import Html exposing (Html)
import Html.Attributes as Attr
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Ui.Css as Css
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography


type alias Layout =
    { height : Int
    , axisWidth : Int
    , plotTop : Float
    , plotBottom : Float
    , plotHeight : Float
    , plotWidth : Float
    , slotWidth : Float
    , barWidth : Float
    }


layout : Int -> Int -> Int -> Int -> Layout
layout availableWidth count height targetSlotWidth =
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


ringDayLabel : Theme -> String -> Html msg
ringDayLabel theme label =
    Html.div
        [ Typography.htmlCaption
        , Attr.style "color" (Css.toCssColor theme.textPrimary)
        ]
        [ Html.text label ]


ringMonthCenter : Theme -> String -> List (Html msg) -> Html msg
ringMonthCenter theme label values =
    Html.div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        , Attr.style "gap" "2px"
        ]
        (Html.div
            [ Typography.htmlCaption
            , Attr.style "font-weight" "600"
            , Attr.style "color" (Css.toCssColor theme.textMuted)
            , Attr.style "line-height" "1"
            ]
            [ Html.text label ]
            :: values
        )


ringValueText : Color -> String -> Html msg
ringValueText color content =
    Html.div
        [ Typography.htmlCaption
        , Attr.style "font-weight" "600"
        , Attr.style "color" (Css.toCssColor color)
        , Attr.style "line-height" "1.25"
        , Attr.style "white-space" "nowrap"
        ]
        [ Html.text content ]


fixedAxis :
    Theme
    -> (Float -> String)
    -> Layout
    -> List Float
    -> (Float -> Float)
    -> Svg msg
    -> Element msg
fixedAxis theme formatTick chartLayout ticks y plot =
    let
        axis =
            Svg.svg
                [ SvgA.viewBox
                    ("0 0 "
                        ++ String.fromInt chartLayout.axisWidth
                        ++ " "
                        ++ String.fromInt chartLayout.height
                    )
                , SvgA.width (String.fromInt chartLayout.axisWidth)
                , SvgA.height (String.fromInt chartLayout.height)
                , SvgA.preserveAspectRatio "none"
                , Attr.attribute "aria-hidden" "true"
                ]
                (List.map
                    (\tick ->
                        Svg.text_
                            [ SvgA.x (String.fromInt (chartLayout.axisWidth - 8))
                            , SvgA.y (float (y tick + 4))
                            , SvgA.textAnchor "end"
                            , SvgA.fill (Css.toCssColor theme.textMuted)
                            , SvgA.fontSize "11"
                            ]
                            [ Svg.text (formatTick tick) ]
                    )
                    ticks
                )
    in
    Element.el [ Element.width Element.fill, Typography.minWidthZero ]
        (Element.html
            (Html.div
                [ Attr.style "display" "grid"
                , Attr.style "grid-template-columns"
                    (String.fromInt chartLayout.axisWidth ++ "px minmax(0, 1fr)")
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


plotAttributes : Layout -> String -> List (Svg.Attribute msg)
plotAttributes chartLayout label =
    [ SvgA.viewBox
        ("0 0 "
            ++ float chartLayout.plotWidth
            ++ " "
            ++ String.fromInt chartLayout.height
        )
    , SvgA.width (float chartLayout.plotWidth)
    , SvgA.height (String.fromInt chartLayout.height)
    , SvgA.preserveAspectRatio "none"
    , Attr.attribute "role" "img"
    , Attr.attribute "aria-label" label
    , Attr.style "display" "block"
    , Attr.style "max-width" "none"
    ]


gridLines : Theme -> Layout -> (Float -> Float) -> List Float -> List (Svg msg)
gridLines theme chartLayout y ticks =
    List.map
        (\tick ->
            Svg.line
                [ SvgA.x1 "0"
                , SvgA.x2 (float chartLayout.plotWidth)
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


activeBand : Theme -> Layout -> Bool -> Float -> Svg msg
activeBand theme chartLayout selected center =
    if selected then
        Svg.rect
            [ SvgA.x (float (center - chartLayout.slotWidth * 0.45))
            , SvgA.y (float chartLayout.plotTop)
            , SvgA.width (float (chartLayout.slotWidth * 0.9))
            , SvgA.height (float chartLayout.plotHeight)
            , SvgA.rx "8"
            , SvgA.fill (Css.toCssColor theme.selectedBg)
            ]
            []

    else
        Svg.g [] []


roundedBar : Color -> Float -> Float -> Float -> Float -> Float -> Svg msg
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


xAxisLabel : Theme -> Layout -> Float -> String -> Svg msg
xAxisLabel theme chartLayout center label =
    Svg.text_
        [ SvgA.x (float center)
        , SvgA.y (float (chartLayout.plotBottom + 28))
        , SvgA.textAnchor "middle"
        , SvgA.fill (Css.toCssColor theme.textMuted)
        , SvgA.fontSize "11"
        ]
        [ Svg.text label ]


hitTarget : Layout -> Float -> String -> String -> msg -> Svg msg
hitTarget chartLayout center period label onSelect =
    Svg.rect
        [ SvgA.x (float (center - chartLayout.slotWidth / 2))
        , SvgA.y (float chartLayout.plotTop)
        , SvgA.width (float chartLayout.slotWidth)
        , SvgA.height (float chartLayout.plotHeight)
        , SvgA.fill "transparent"
        , SvgA.cursor "pointer"
        , Attr.tabindex 0
        , Attr.attribute "data-period" period
        , Attr.attribute "aria-label" label
        , Attr.attribute "role" "button"
        , SvgE.onClick onSelect
        ]
        []


marker : Theme -> Color -> Float -> Float -> Svg msg
marker theme color x y =
    Svg.circle
        [ SvgA.cx (float x)
        , SvgA.cy (float y)
        , SvgA.r "4.5"
        , SvgA.fill (Css.toCssColor theme.cardBg)
        , SvgA.stroke (Css.toCssColor color)
        , SvgA.strokeWidth "3"
        ]
        []


float : Float -> String
float =
    String.fromFloat
