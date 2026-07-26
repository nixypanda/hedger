module Ui.Chart.Donut exposing (view)

import Element exposing (Element)
import Html.Attributes as Attr
import Path
import Shape
import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Ui.Css as Css


view :
    { size : Int
    , outerRadius : Float
    , innerRadius : Float
    , padAngle : Float
    , cornerRadius : Float
    , selectedOffset : Float
    , label : String
    , items : List item
    , value : item -> Float
    , color : Int -> item -> Element.Color
    , itemLabel : item -> String
    , selected : Maybe Int
    , onSelect : Int -> msg
    , backgroundColor : Element.Color
    }
    -> Element msg
view config =
    let
        values =
            List.map (config.value >> abs) config.items

        pieData =
            Shape.pie
                { outerRadius = config.outerRadius
                , innerRadius = config.innerRadius
                , padAngle = config.padAngle
                , cornerRadius = config.cornerRadius
                , startAngle = 0
                , endAngle = 2 * pi
                , padRadius = 0
                , valueFn = identity
                , sortingFn = \_ _ -> EQ
                }
                values

        renderSlice index item arc =
            let
                isSelected =
                    config.selected == Just index

                midAngle =
                    (arc.startAngle + arc.endAngle) / 2

                offset =
                    if isSelected then
                        config.selectedOffset

                    else
                        0
            in
            Svg.g
                [ SvgA.transform
                    ("translate "
                        ++ String.fromFloat (offset * cos midAngle)
                        ++ " "
                        ++ String.fromFloat (offset * sin midAngle)
                    )
                , SvgA.cursor "pointer"
                , SvgA.style "transition: transform 150ms ease"
                , SvgE.onClick (config.onSelect index)
                , Attr.tabindex 0
                , Attr.attribute "role" "button"
                , Attr.attribute "aria-label" (config.itemLabel item)
                ]
                [ Path.element (Shape.arc arc)
                    [ SvgA.fill (Css.toCssColor (config.color index item))
                    , SvgA.stroke (Css.toCssColor config.backgroundColor)
                    , SvgA.strokeWidth
                        (if isSelected then
                            "3"

                         else
                            "1"
                        )
                    ]
                ]

        indexedSlices =
            List.map2 Tuple.pair config.items pieData
                |> List.indexedMap (\index ( item, arc ) -> ( index, item, arc ))

        ( selectedSlices, unselectedSlices ) =
            List.partition
                (\( index, _, _ ) -> config.selected == Just index)
                indexedSlices

        center =
            toFloat config.size / 2
    in
    Element.el
        [ Element.width (Element.px config.size)
        , Element.height (Element.px config.size)
        ]
        (Element.html
            (Svg.svg
                [ SvgA.viewBox
                    ("0 0 " ++ String.fromInt config.size ++ " " ++ String.fromInt config.size)
                , SvgA.width (String.fromInt config.size)
                , SvgA.height (String.fromInt config.size)
                , SvgA.style "overflow: visible"
                , Attr.attribute "role" "img"
                , Attr.attribute "aria-label" config.label
                ]
                [ Svg.g
                    [ SvgA.transform
                        ("translate(" ++ String.fromFloat center ++ "," ++ String.fromFloat center ++ ")")
                    ]
                    (List.map
                        (\( index, item, arc ) -> renderSlice index item arc)
                        (unselectedSlices ++ selectedSlices)
                    )
                ]
            )
        )
