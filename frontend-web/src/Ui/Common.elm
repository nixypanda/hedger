module Ui.Common exposing
    ( hoverBackground
    , hoverBorder
    , minWidthZero
    , toCssColor
    , touchTargetHeight
    , truncatedText
    , uiClass
    )

import Element exposing (Element)
import Html.Attributes
import Ui.DesignTokens exposing (minimumTouchTarget)


uiClass : String -> Element.Attribute msg
uiClass className =
    Element.htmlAttribute (Html.Attributes.class className)


minWidthZero : Element.Attribute msg
minWidthZero =
    Element.htmlAttribute (Html.Attributes.style "min-width" "0")


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


hoverBackground : Element.Color -> List (Element.Attribute msg)
hoverBackground color =
    [ Element.htmlAttribute (Html.Attributes.class "ui-hover-background")
    , Element.htmlAttribute
        (Html.Attributes.style "--ui-hover-background" (toCssColor color))
    ]


hoverBorder : Element.Color -> List (Element.Attribute msg)
hoverBorder color =
    [ Element.htmlAttribute (Html.Attributes.class "ui-hover-border")
    , Element.htmlAttribute
        (Html.Attributes.style "--ui-hover-border" (toCssColor color))
    ]
