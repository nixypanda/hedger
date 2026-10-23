module Ui.Surface exposing
    ( accentDot
    , card
    , cardWith
    , formSection
    , hairline
    , rowSurfaceAttrs
    , section
    )

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Ui.Common as Common
import Ui.DesignTokens exposing (radiusCard, radiusControl, radiusHairline, space12, space20)
import Ui.Theme exposing (Theme)
import Ui.Typography exposing (subsectionTitle)


card : Theme -> List (Element msg) -> Element msg
card theme children =
    cardWith [] theme children


cardWith : List (Attribute msg) -> Theme -> List (Element msg) -> Element msg
cardWith attributes theme children =
    Element.column
        (attributes
            ++ [ Element.width Element.fill
               , Background.color theme.cardBg
               , Border.rounded radiusCard
               , Element.padding space20
               , Element.spacing space12
               , Border.width 1
               , Border.color theme.divider
               , Common.uiClass "ui-card-shadow ui-surface"
               ]
        )
        children


section :
    Theme
    ->
        { header : Maybe (Element msg)
        , body : List (Element msg)
        , footer : Maybe (Element msg)
        }
    -> Element msg
section theme config =
    card theme
        (optionalWithDivider theme config.header
            ++ config.body
            ++ optionalFooter theme config.footer
        )


formSection : Theme -> String -> List (Element msg) -> Element msg
formSection theme title children =
    section theme
        { header = Just (subsectionTitle theme title)
        , body = children
        , footer = Nothing
        }


hairline : Theme -> Element msg
hairline theme =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px 1)
        , Background.color theme.divider
        ]
        Element.none


rowSurfaceAttrs : Theme -> Bool -> List (Attribute msg)
rowSurfaceAttrs theme selected =
    [ Element.width Element.fill
    , Element.padding space12
    , Background.color
        (if selected then
            theme.selectedBg

         else
            theme.cardBg
        )
    , Border.rounded radiusControl
    , Border.width 1
    , Border.color
        (if selected then
            theme.primarySoft

         else
            theme.divider
        )
    ]
        ++ Common.hoverBackground theme.hoverBg
        ++ [ Common.uiClass "ui-motion" ]


accentDot : Element.Color -> Element msg
accentDot color =
    Element.el
        [ Element.width (Element.px 4)
        , Element.height (Element.px 24)
        , Background.color color
        , Border.rounded radiusHairline
        ]
        Element.none


optionalWithDivider : Theme -> Maybe (Element msg) -> List (Element msg)
optionalWithDivider theme maybeElement =
    case maybeElement of
        Just element ->
            [ element, hairline theme ]

        Nothing ->
            []


optionalFooter : Theme -> Maybe (Element msg) -> List (Element msg)
optionalFooter theme maybeElement =
    case maybeElement of
        Just element ->
            [ hairline theme, element ]

        Nothing ->
            []
