module ThemeTest exposing (suite)

import Element exposing (Color)
import Expect
import Test exposing (Test, describe, test)
import Ui.Theme as Theme exposing (Mode(..), Theme)


suite : Test
suite =
    describe "theme contrast"
        (List.map contrastTest contrastCases)


type alias ContrastCase =
    { name : String
    , foreground : Color
    , background : Color
    }


contrastCases : List ContrastCase
contrastCases =
    List.concatMap themeContrastCases
        [ ( "light", Theme.light )
        , ( "dark", Theme.forMode Dark )
        ]


themeContrastCases : ( String, Theme ) -> List ContrastCase
themeContrastCases ( mode, theme ) =
    [ contrastCase mode "muted text on cards" theme.textMuted theme.cardBg
    , contrastCase mode "muted text on page" theme.textMuted theme.pageBg
    , contrastCase mode "primary button" theme.primaryButtonFg theme.primary
    , contrastCase mode "primary button hover" theme.primaryButtonFg theme.primaryPressed
    , contrastCase mode "danger button" theme.dangerSolidFg theme.dangerSolid
    , contrastCase mode "danger button hover" theme.dangerSolidFg theme.dangerFg
    , contrastCase mode "neutral badge" theme.neutral theme.chipBg
    ]


contrastCase : String -> String -> Color -> Color -> ContrastCase
contrastCase mode label foreground background =
    { name = mode ++ " " ++ label
    , foreground = foreground
    , background = background
    }


contrastTest : ContrastCase -> Test
contrastTest contrast =
    test contrast.name <|
        \_ ->
            contrastRatio contrast.foreground contrast.background
                |> Expect.atLeast 4.5


contrastRatio : Color -> Color -> Float
contrastRatio foreground background =
    let
        foregroundLuminance =
            relativeLuminance foreground

        backgroundLuminance =
            relativeLuminance background
    in
    (max foregroundLuminance backgroundLuminance + 0.05)
        / (min foregroundLuminance backgroundLuminance + 0.05)


relativeLuminance : Color -> Float
relativeLuminance color =
    let
        channels =
            Element.toRgb color
    in
    0.2126
        * linearChannel channels.red
        + 0.7152
        * linearChannel channels.green
        + 0.0722
        * linearChannel channels.blue


linearChannel : Float -> Float
linearChannel channel =
    if channel <= 0.04045 then
        channel / 12.92

    else
        ((channel + 0.055) / 1.055) ^ 2.4
