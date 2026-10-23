module Ui.Chart.Sparkline exposing (view)

import Element exposing (Element)
import Html.Attributes as Attr
import Svg
import Svg.Attributes as SvgA
import Ui.Common as Common


view :
    { width : Int
    , height : Int
    , color : Element.Color
    , label : String
    , values : List Float
    }
    -> Element msg
view config =
    let
        low =
            List.minimum config.values |> Maybe.withDefault 0

        high =
            List.maximum config.values |> Maybe.withDefault 0

        spread =
            max 0.000001 (high - low)

        count =
            List.length config.values

        point index value =
            let
                x =
                    if count <= 1 then
                        toFloat config.width / 2

                    else
                        3 + (toFloat index / toFloat (count - 1) * toFloat (config.width - 6))

                y =
                    3 + ((high - value) / spread * toFloat (config.height - 6))
            in
            String.fromFloat x ++ "," ++ String.fromFloat y
    in
    if List.isEmpty config.values then
        Element.none

    else
        Element.el
            [ Element.width (Element.px config.width)
            , Element.height (Element.px config.height)
            , Element.alignRight
            ]
            (Element.html
                (Svg.svg
                    [ SvgA.viewBox
                        ("0 0 "
                            ++ String.fromInt config.width
                            ++ " "
                            ++ String.fromInt config.height
                        )
                    , SvgA.width "100%"
                    , SvgA.height (String.fromInt config.height)
                    , Attr.attribute "role" "img"
                    , Attr.attribute "aria-label" config.label
                    ]
                    [ Svg.polyline
                        [ SvgA.points (String.join " " (List.indexedMap point config.values))
                        , SvgA.fill "none"
                        , SvgA.stroke (Common.toCssColor config.color)
                        , SvgA.strokeWidth "2"
                        , SvgA.strokeLinecap "round"
                        , SvgA.strokeLinejoin "round"
                        ]
                        []
                    ]
                )
            )
