module Ui.Color exposing (mix, withAlpha)

import Element exposing (Color)


mix : Color -> Color -> Float -> Color
mix start finish ratio =
    let
        from =
            Element.toRgb start

        to =
            Element.toRgb finish

        amount =
            clamp 0 1 ratio

        blend first second =
            first + (second - first) * amount
    in
    Element.rgba
        (blend from.red to.red)
        (blend from.green to.green)
        (blend from.blue to.blue)
        (blend from.alpha to.alpha)


withAlpha : Float -> Color -> Color
withAlpha alpha color =
    let
        rgb =
            Element.toRgb color
    in
    Element.rgba rgb.red rgb.green rgb.blue alpha
