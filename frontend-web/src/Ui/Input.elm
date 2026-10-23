module Ui.Input exposing
    ( dateInput
    , fieldLabel
    , searchInputInline
    , segmentedControl
    , segmentedOption
    , wideSegmentedControl
    )

import Element exposing (Attribute, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Ui.Common as Common
import Ui.DesignTokens
    exposing
        ( compactControlVisualHeight
        , radiusControl
        , radiusSmall
        , space12
        , space2
        , space4
        )
import Ui.Theme exposing (Theme)
import Ui.Typography exposing (caption, controlLabel)


fieldVerticalPadding : Int
fieldVerticalPadding =
    14


fieldLabel : Theme -> String -> Element msg
fieldLabel theme label =
    Element.el
        [ caption
        , Font.medium
        , Font.color theme.textPrimary
        ]
        (Element.text label)


dateInput :
    Theme
    ->
        { label : String
        , value : String
        , onChange : String -> msg
        }
    -> Element msg
dateInput theme config =
    Input.text
        (Element.htmlAttribute (Html.Attributes.type_ "date") :: inputAttrs theme)
        { onChange = config.onChange
        , text = config.value
        , placeholder = Nothing
        , label = Input.labelAbove [] (fieldLabel theme config.label)
        }


searchInputInline :
    Theme
    ->
        { label : String
        , text : String
        , onChange : String -> msg
        , extraAttrs : List (Attribute msg)
        }
    -> Element msg
searchInputInline theme config =
    Input.text (inputAttrs theme ++ config.extraAttrs)
        { onChange = config.onChange
        , text = config.text
        , placeholder = Nothing
        , label = Input.labelHidden config.label
        }


segmentedControl : Theme -> List (Element msg) -> Element msg
segmentedControl theme segments =
    Element.row
        [ Element.spacing space2
        , Element.padding space4
        , Border.rounded radiusControl
        , Background.color theme.chipBg
        , Border.width 1
        , Border.color theme.divider
        ]
        segments


wideSegmentedControl :
    Theme
    -> List { label : String, selected : Bool, onPress : msg }
    -> Element msg
wideSegmentedControl theme options =
    Element.row
        [ Element.width Element.fill
        , Element.spacing space2
        , Element.padding space4
        , Border.rounded radiusControl
        , Background.color theme.chipBg
        , Border.width 1
        , Border.color theme.divider
        ]
        (List.map
            (\config ->
                Input.button
                    [ Element.width (Element.fillPortion 1)
                    , Font.color
                        (if config.selected then
                            theme.primary

                         else
                            theme.textMedium
                        )
                    , Font.medium
                    , Font.center
                    , controlLabel
                    , Common.uiClass "ui-motion"
                    , Element.htmlAttribute
                        (Html.Attributes.attribute
                            "aria-pressed"
                            (if config.selected then
                                "true"

                             else
                                "false"
                            )
                        )
                    ]
                    { onPress = Just config.onPress
                    , label = segmentedSurface theme config.selected config.label
                    }
            )
            options
        )


segmentedOption :
    Theme
    -> { label : String, selected : Bool, onPress : msg }
    -> Element msg
segmentedOption theme config =
    Input.button
        [ Common.touchTargetHeight
        , Font.color
            (if config.selected then
                theme.primary

             else
                theme.textMedium
            )
        , Font.medium
        , Font.center
        , controlLabel
        , Common.uiClass "ui-motion"
        , Element.htmlAttribute
            (Html.Attributes.attribute
                "aria-pressed"
                (if config.selected then
                    "true"

                 else
                    "false"
                )
            )
        ]
        { onPress = Just config.onPress
        , label = segmentedSurface theme config.selected config.label
        }


segmentedSurface : Theme -> Bool -> String -> Element msg
segmentedSurface theme selected label =
    Element.el
        [ Element.width Element.fill
        , Element.height (Element.px compactControlVisualHeight)
        , Element.paddingXY space12 0
        , Border.rounded radiusSmall
        , Background.color
            (if selected then
                theme.cardBg

             else
                theme.chipBg
            )
        ]
        (Element.el [ Element.centerX, Element.centerY ] (Element.text label))


inputAttrs : Theme -> List (Attribute msg)
inputAttrs theme =
    Element.paddingXY space12 fieldVerticalPadding :: inputBaseAttrs theme


inputBaseAttrs : Theme -> List (Attribute msg)
inputBaseAttrs theme =
    [ Element.width Element.fill
    , Border.rounded radiusControl
    , Border.width 1
    , Border.color theme.divider
    , Background.color theme.inputFill
    , Font.color theme.textPrimary
    , Common.uiClass "ui-motion"
    ]
