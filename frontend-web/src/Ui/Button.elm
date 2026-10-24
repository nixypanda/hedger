module Ui.Button exposing
    ( backButton
    , dangerButton
    , ghostButton
    , iconButton
    , primaryButton
    , refreshButton
    , secondaryButton
    )

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html.Attributes
import Ui.Common as Common
import Ui.DesignTokens
    exposing
        ( minimumTouchTarget
        , radiusControl
        , radiusPill
        )
import Ui.Icon as Icon exposing (Icon)
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography


primaryButton : Theme -> { label : String, onPress : msg } -> Element msg
primaryButton theme config =
    buttonBase theme
        { label = Element.text config.label
        , onPress = Just config.onPress
        , background = theme.primary
        , foreground = theme.primaryButtonFg
        , border = theme.primary
        , hoverBackground = theme.primaryPressed
        }


secondaryButton : Theme -> { label : String, onPress : msg } -> Element msg
secondaryButton theme config =
    buttonBase theme
        { label = Element.text config.label
        , onPress = Just config.onPress
        , background = theme.primarySoft
        , foreground = theme.primary
        , border = theme.primarySoft
        , hoverBackground = theme.selectedBg
        }


dangerButton : Theme -> { label : String, onPress : msg } -> Element msg
dangerButton theme config =
    buttonBase theme
        { label = Element.text config.label
        , onPress = Just config.onPress
        , background = theme.dangerSolid
        , foreground = theme.dangerSolidFg
        , border = theme.dangerSolid
        , hoverBackground = theme.dangerFg
        }


ghostButton : Theme -> { label : String, onPress : msg } -> Element msg
ghostButton theme config =
    ghostButtonWithLabel theme
        { label = Element.text config.label
        , onPress = config.onPress
        }


ghostButtonWithLabel : Theme -> { label : Element msg, onPress : msg } -> Element msg
ghostButtonWithLabel theme config =
    buttonBase theme
        { label = config.label
        , onPress = Just config.onPress
        , background = theme.cardBg
        , foreground = theme.textPrimary
        , border = theme.divider
        , hoverBackground = theme.hoverBg
        }


iconButton : Theme -> { icon : Icon, title : String, onPress : msg } -> Element msg
iconButton theme config =
    iconButtonBase theme
        { icon = config.icon
        , title = config.title
        , onPress = Just config.onPress
        , background = theme.cardBg
        , foreground = theme.textPrimary
        , border = theme.divider
        , hoverBackground = theme.primarySoft
        , hoverBorder = theme.primary
        }


backButton : Theme -> { onPress : msg } -> Element msg
backButton theme config =
    iconButton theme
        { icon = Icon.ChevronLeft
        , title = "Back"
        , onPress = config.onPress
        }


refreshButton : Theme -> { loading : Bool, onPress : msg } -> Element msg
refreshButton theme config =
    let
        title =
            if config.loading then
                "Refreshing"

            else
                "Refresh"

        icon =
            if config.loading then
                Icon.LoaderCircle

            else
                Icon.Refresh

        iconView =
            Element.el
                (Element.centerX
                    :: Element.centerY
                    :: (if config.loading then
                            [ Common.uiClass "ui-spin" ]

                        else
                            []
                       )
                )
                (Icon.view
                    { icon = icon
                    , size = 20
                    , color = theme.textPrimary
                    }
                )
    in
    Input.button
        ([ Element.width (Element.px minimumTouchTarget)
         , Element.height (Element.px minimumTouchTarget)
         , Background.color theme.cardBg
         , Font.color theme.textPrimary
         , Font.center
         , Border.rounded radiusPill
         , Border.width 1
         , Border.color theme.divider
         , Common.uiClass "ui-motion"
         ]
            ++ Common.hoverBackground theme.primarySoft
            ++ Common.hoverBorder theme.primary
            ++ [ Element.htmlAttribute (Html.Attributes.title title)
               , Element.htmlAttribute (Html.Attributes.attribute "aria-label" title)
               , Element.htmlAttribute
                    (Html.Attributes.attribute "aria-busy"
                        (if config.loading then
                            "true"

                         else
                            "false"
                        )
                    )
               , Element.htmlAttribute
                    (Html.Attributes.attribute "aria-disabled"
                        (if config.loading then
                            "true"

                         else
                            "false"
                        )
                    )
               , Region.description title
               ]
            ++ (if config.loading then
                    [ Element.htmlAttribute (Html.Attributes.attribute "inert" "") ]

                else
                    []
               )
        )
        { onPress =
            if config.loading then
                Nothing

            else
                Just config.onPress
        , label = iconView
        }


buttonBase :
    Theme
    ->
        { label : Element msg
        , onPress : Maybe msg
        , background : Element.Color
        , foreground : Element.Color
        , border : Element.Color
        , hoverBackground : Element.Color
        }
    -> Element msg
buttonBase _ config =
    Input.button
        ([ Element.paddingEach { top = 11, right = 16, bottom = 11, left = 16 }
         , Border.rounded radiusControl
         , Border.width 1
         , Border.color config.border
         , Background.color config.background
         , Font.color config.foreground
         , Font.center
         , Common.uiClass "ui-motion"
         ]
            ++ Typography.label
            ++ Common.hoverBackground config.hoverBackground
            ++ [ Element.height (Element.shrink |> Element.minimum minimumTouchTarget) ]
        )
        { onPress = config.onPress
        , label = Element.el [ Element.centerX ] config.label
        }


iconButtonBase :
    Theme
    ->
        { icon : Icon
        , title : String
        , onPress : Maybe msg
        , background : Element.Color
        , foreground : Element.Color
        , border : Element.Color
        , hoverBackground : Element.Color
        , hoverBorder : Element.Color
        }
    -> Element msg
iconButtonBase _ config =
    Input.button
        ([ Element.width (Element.px minimumTouchTarget)
         , Element.height (Element.px minimumTouchTarget)
         , Background.color config.background
         , Font.color config.foreground
         , Font.center
         , Border.rounded radiusPill
         , Border.width 1
         , Border.color config.border
         , Common.uiClass "ui-motion"
         ]
            ++ Common.hoverBackground config.hoverBackground
            ++ Common.hoverBorder config.hoverBorder
            ++ [ Element.htmlAttribute (Html.Attributes.title config.title)
               , Region.description config.title
               ]
        )
        { onPress = config.onPress
        , label =
            Icon.view
                { icon = config.icon
                , size = 20
                , color = config.foreground
                }
        }
