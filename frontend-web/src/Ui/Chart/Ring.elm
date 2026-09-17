module Ui.Chart.Ring exposing
    ( Segment
    , Size(..)
    , view
    )

import Element exposing (Element)
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as Attr
import Ui.Css as Css


type Size
    = Pixels Int
    | Fill
        { minimum : Int
        , maximum : Int
        }


type alias Segment =
    { color : Element.Color
    , value : Float
    }


view :
    { size : Size
    , radius : Float
    , thickness : Int
    , gap : Float
    , segments : List Segment
    , emptyColor : Element.Color
    , centerColor : Element.Color
    , selected : Bool
    , onPress : Maybe msg
    , label : Maybe String
    , center : Html msg
    }
    -> Element msg
view config =
    let
        ring =
            Html.div
                (sizeStyles config.size
                    ++ [ Attr.style "border-radius" "50%"
                       , Attr.style "background" (gradient config)
                       , Attr.style "display" "grid"
                       , Attr.style "place-items" "center"
                       , Css.htmlClass "ui-chart-motion"
                       , Attr.style "transform"
                            (if config.selected then
                                "scale(1.05)"

                             else
                                "scale(1)"
                            )
                       ]
                    ++ accessibilityAttributes config.label
                )
                [ Html.div
                    [ Attr.style "width"
                        ("calc(100% - " ++ String.fromInt (config.thickness * 2) ++ "px)")
                    , Attr.style "height"
                        ("calc(100% - " ++ String.fromInt (config.thickness * 2) ++ "px)")
                    , Attr.style "border-radius" "50%"
                    , Attr.style "background" (Css.toCssColor config.centerColor)
                    , Attr.style "display" "grid"
                    , Attr.style "place-items" "center"
                    ]
                    [ config.center ]
                ]

        element =
            Element.html ring
    in
    case config.onPress of
        Nothing ->
            element

        Just onPress ->
            Input.button (elementAttributes config.size config.label)
                { onPress = Just onPress
                , label = element
                }


gradient :
    { config
        | radius : Float
        , gap : Float
        , segments : List Segment
        , emptyColor : Element.Color
    }
    -> String
gradient config =
    let
        positiveSegments =
            List.filter (.value >> (\value -> value > 0)) config.segments

        total =
            List.sum (List.map .value positiveSegments)

        gapSize =
            config.gap / (2 * pi * config.radius) * 100

        gapHalf =
            gapSize / 2

        step segment ( offset, rendered ) =
            let
                next =
                    offset + segment.value / total * 100

                segmentStart =
                    offset + gapHalf

                segmentEnd =
                    next - gapHalf

                color =
                    Css.toCssColor segment.color
            in
            ( next
            , if segmentEnd > segmentStart then
                rendered
                    ++ [ color ++ " " ++ float segmentStart ++ "%"
                       , color ++ " " ++ float segmentEnd ++ "%"
                       , "transparent " ++ float segmentEnd ++ "%"
                       , "transparent " ++ float (next + gapHalf) ++ "%"
                       ]

              else
                rendered
            )

        stops =
            positiveSegments
                |> List.foldl step ( 0, [] )
                |> Tuple.second
    in
    if total <= 0 || List.isEmpty stops then
        Css.toCssColor config.emptyColor

    else
        "conic-gradient(" ++ String.join ", " stops ++ ")"


sizeStyles : Size -> List (Html.Attribute msg)
sizeStyles size =
    case size of
        Pixels pixels ->
            [ Attr.style "width" (String.fromInt pixels ++ "px")
            , Attr.style "height" (String.fromInt pixels ++ "px")
            ]

        Fill _ ->
            [ Attr.style "width" "100%"
            , Attr.style "height" "100%"
            ]


elementAttributes : Size -> Maybe String -> List (Element.Attribute msg)
elementAttributes size label =
    let
        labelAttribute =
            label
                |> Maybe.map (Attr.attribute "aria-label" >> Element.htmlAttribute)
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
    in
    case size of
        Pixels pixels ->
            [ Element.width (Element.px pixels)
            , Element.height (Element.px pixels)
            ]
                ++ labelAttribute

        Fill limits ->
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.htmlAttribute (Attr.style "min-width" (String.fromInt limits.minimum ++ "px"))
            , Element.htmlAttribute (Attr.style "max-width" (String.fromInt limits.maximum ++ "px"))
            , Element.htmlAttribute (Attr.style "aspect-ratio" "1")
            ]
                ++ labelAttribute


accessibilityAttributes : Maybe String -> List (Html.Attribute msg)
accessibilityAttributes label =
    case label of
        Nothing ->
            [ Attr.attribute "aria-hidden" "true" ]

        Just value ->
            [ Attr.attribute "aria-label" value ]


float : Float -> String
float =
    String.fromFloat
