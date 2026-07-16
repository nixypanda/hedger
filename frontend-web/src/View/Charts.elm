module View.Charts exposing (Point, barChart)

import Element exposing (Element)
import Html.Attributes
import Svg exposing (rect, svg, text_)
import Svg.Attributes as SvgA
import Ui.Theme exposing (Theme)


type alias Point =
    { label : String
    , value : Float
    }


barChart : Theme -> List Point -> Element msg
barChart theme points =
    let
        width =
            640

        height =
            220

        plotHeight =
            170

        maximum =
            points
                |> List.map (.value >> abs)
                |> List.maximum
                |> Maybe.withDefault 1
                |> max 1

        count =
            max 1 (List.length points)

        slotWidth =
            toFloat width / toFloat count

        barWidth =
            min 54 (slotWidth * 0.62)

        viewPoint index point =
            let
                barHeight =
                    abs point.value / maximum * toFloat plotHeight

                x =
                    toFloat index * slotWidth + (slotWidth - barWidth) / 2

                y =
                    toFloat plotHeight - barHeight
            in
            Svg.g []
                [ rect
                    [ SvgA.x (String.fromFloat x)
                    , SvgA.y (String.fromFloat y)
                    , SvgA.width (String.fromFloat barWidth)
                    , SvgA.height (String.fromFloat barHeight)
                    , SvgA.rx "6"
                    , SvgA.fill (cssColor theme.primary)
                    ]
                    []
                , text_
                    [ SvgA.x (String.fromFloat (x + barWidth / 2))
                    , SvgA.y "205"
                    , SvgA.textAnchor "middle"
                    , SvgA.fontSize "12"
                    , SvgA.fill (cssColor theme.textMuted)
                    ]
                    [ Svg.text point.label ]
                ]
    in
    svg
        [ SvgA.viewBox ("0 0 " ++ String.fromInt width ++ " " ++ String.fromInt height)
        , SvgA.width "100%"
        , SvgA.height (String.fromInt height)
        , Html.Attributes.attribute "role" "img"
        , Html.Attributes.attribute "aria-label" "Example bar chart"
        ]
        (List.indexedMap viewPoint points)
        |> Element.html


cssColor : Element.Color -> String
cssColor color =
    let
        rgb =
            Element.toRgb color
    in
    "rgba("
        ++ String.fromInt (round (rgb.red * 255))
        ++ ","
        ++ String.fromInt (round (rgb.green * 255))
        ++ ","
        ++ String.fromInt (round (rgb.blue * 255))
        ++ ","
        ++ String.fromFloat rgb.alpha
        ++ ")"
