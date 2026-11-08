module Ui.Input exposing
    ( Segment
    , Segments
    , dateInput
    , fieldLabel
    , searchInputInline
    , segmentedControl
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
        , minimumTouchTarget
        , radiusControl
        , radiusSmall
        , space12
        , space2
        , space4
        )
import Ui.Motion as Motion
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography


fieldVerticalPadding : Int
fieldVerticalPadding =
    14


fieldLabel : Theme -> String -> Element msg
fieldLabel theme label =
    Element.el
        (Typography.labelSmall ++ [ Font.color theme.textPrimary ])
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


type alias Segment value =
    { label : String, value : value }


type alias Segments value =
    { first : Segment value, rest : List (Segment value) }


{-| A labelled group of toggle buttons with exactly one selected value.
Empty options are impossible by construction; a value absent from the options
renders no indicator and leaves every option unselected.
-}
segmentedControl :
    Theme
    ->
        { label : String
        , selected : value
        , options : Segments value
        , onSelect : value -> msg
        }
    -> Element msg
segmentedControl theme config =
    let
        allOptions =
            config.options.first :: config.options.rest

        maybeIndex =
            findSelectedIndex config.selected config.options
    in
    Element.row
        [ Common.uiClass "ui-segmented ui-motion"
        , Element.htmlAttribute (Html.Attributes.attribute "role" "group")
        , Element.htmlAttribute
            (Html.Attributes.attribute "aria-label" config.label)
        , Element.htmlAttribute
            (Html.Attributes.style "gap" (String.fromInt space2 ++ "px"))
        , Element.padding space4
        , Border.rounded radiusControl
        , Background.color theme.chipBg
        , Border.width 1
        , Border.color theme.divider
        ]
        ((case maybeIndex of
            Just index ->
                [ segmentedIndicator index ]

            Nothing ->
                []
         )
            ++ List.indexedMap
                (segmentedOption theme config.onSelect maybeIndex)
                allOptions
        )


findSelectedIndex : value -> Segments value -> Maybe Int
findSelectedIndex selected options =
    (options.first :: options.rest)
        |> List.indexedMap Tuple.pair
        |> List.filterMap
            (\( index, option ) ->
                if option.value == selected then
                    Just index

                else
                    Nothing
            )
        |> List.head


segmentedIndicator : Int -> Element msg
segmentedIndicator index =
    Element.el
        [ Common.uiClass "ui-segment-indicator"
        , Element.htmlAttribute (Html.Attributes.attribute "aria-hidden" "true")
        , Element.width Element.fill
        , Element.height (Element.px compactControlVisualHeight)
        , Border.rounded radiusSmall
        , Element.htmlAttribute
            (Html.Attributes.style
                "transform"
                ("translateX(calc("
                    ++ String.fromInt index
                    ++ " * (100% + "
                    ++ String.fromInt space2
                    ++ "px)))"
                )
            )
        ]
        Element.none


segmentedOption :
    Theme
    -> (value -> msg)
    -> Maybe Int
    -> Int
    -> Segment value
    -> Element msg
segmentedOption theme onSelect maybeIndex index option =
    Input.button
        (Typography.label
            ++ [ Common.touchTargetHeight
               , Element.width (Element.fill |> Element.minimum minimumTouchTarget)
               , Common.uiClass "ui-segment ui-motion"
               , Font.color
                    (if Just index == maybeIndex then
                        theme.primary

                     else
                        theme.textMedium
                    )
               , Font.center
               , Element.htmlAttribute
                    (Html.Attributes.style "grid-column" (String.fromInt (index + 1)))
               , Motion.pressable
               , Element.htmlAttribute
                    (Html.Attributes.attribute
                        "aria-pressed"
                        (if Just index == maybeIndex then
                            "true"

                         else
                            "false"
                        )
                    )
               ]
        )
        { onPress = Just (onSelect option.value)
        , label = segmentedLabel option.label
        }


segmentedLabel : String -> Element msg
segmentedLabel label =
    Element.el
        [ Element.height (Element.px compactControlVisualHeight)
        , Element.paddingXY space12 0
        , Element.centerX
        , Element.centerY
        ]
        (Element.text label)


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
