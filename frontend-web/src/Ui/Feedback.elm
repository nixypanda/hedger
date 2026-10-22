module Ui.Feedback exposing
    ( emptyState
    , errorBanner
    , loading
    )

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Ui.DesignTokens exposing (radiusControl, space12, space8)
import Ui.Icon as Icon
import Ui.Surface as Surface
import Ui.Theme exposing (Theme)


loading : Theme -> String -> Element msg
loading theme message =
    Element.row
        [ Element.spacing space8
        , Font.color theme.textMuted
        ]
        [ Icon.view { icon = Icon.LoaderCircle, size = 18, color = theme.textMuted }
        , Element.text message
        ]


emptyState : Theme -> String -> Element msg
emptyState theme message =
    Surface.card theme
        [ Element.paragraph
            [ Font.color theme.textMuted
            , Font.center
            , Element.padding space8
            ]
            [ Element.text message ]
        ]


errorBanner : Theme -> String -> Element msg
errorBanner theme message =
    Element.row
        [ Element.width Element.fill
        , Element.spacing space8
        , Element.padding space12
        , Background.color theme.dangerBg
        , Font.color theme.dangerFg
        , Border.rounded radiusControl
        ]
        [ Icon.view { icon = Icon.AlertTriangle, size = 18, color = theme.dangerFg }
        , Element.paragraph [ Element.width Element.fill ] [ Element.text message ]
        ]
