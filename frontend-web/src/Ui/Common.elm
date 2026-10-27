module Ui.Common exposing
    ( HoverBackground(..)
    , gridCellMaxWidth
    , hoverBackground
    , hoverBorderPrimary
    , minHeightZero
    , minWidthZero
    , toCssColor
    , touchTargetHeight
    , truncatedText
    , uiClass
    )

import Element exposing (Element)
import Html.Attributes
import Ui.DesignTokens exposing (minimumTouchTarget, space12)


uiClass : String -> Element.Attribute msg
uiClass className =
    Element.htmlAttribute (Html.Attributes.class className)


minWidthZero : Element.Attribute msg
minWidthZero =
    Element.htmlAttribute (Html.Attributes.style "min-width" "0")


minHeightZero : Element.Attribute msg
minHeightZero =
    Element.htmlAttribute (Html.Attributes.style "min-height" "0")


{-| Cap a flex child at one equal column of a row with the given number of
columns and standard space12 gaps. This keeps short final rows aligned without
rendering placeholder cells.
-}
gridCellMaxWidth : Int -> Element.Attribute msg
gridCellMaxWidth columns =
    Element.htmlAttribute
        (Html.Attributes.style
            "max-width"
            ("calc((100% - "
                ++ String.fromInt ((columns - 1) * space12)
                ++ "px) / "
                ++ String.fromInt columns
                ++ ")"
            )
        )


toCssColor : Element.Color -> String
toCssColor color =
    let
        { red, green, blue, alpha } =
            Element.toRgb color
    in
    "rgba("
        ++ String.fromInt (round (red * 255))
        ++ ", "
        ++ String.fromInt (round (green * 255))
        ++ ", "
        ++ String.fromInt (round (blue * 255))
        ++ ", "
        ++ String.fromFloat alpha
        ++ ")"


touchTargetHeight : Element.Attribute msg
touchTargetHeight =
    Element.height (Element.shrink |> Element.minimum minimumTouchTarget)


truncatedText : String -> Element msg
truncatedText value =
    Element.el
        [ Element.width Element.fill
        , Element.clip
        , minWidthZero
        , uiClass "ui-text-ellipsis"
        ]
        (Element.text value)


type HoverBackground
    = HoverPrimary
    | HoverPrimarySoft
    | HoverSelected
    | HoverDanger
    | HoverSurface


hoverBackground : HoverBackground -> List (Element.Attribute msg)
hoverBackground tone =
    [ uiClass <|
        case tone of
            HoverPrimary ->
                "ui-hover-primary"

            HoverPrimarySoft ->
                "ui-hover-primary-soft"

            HoverSelected ->
                "ui-hover-selected"

            HoverDanger ->
                "ui-hover-danger"

            HoverSurface ->
                "ui-hover-surface"
    ]


hoverBorderPrimary : List (Element.Attribute msg)
hoverBorderPrimary =
    [ uiClass "ui-hover-border-primary" ]
