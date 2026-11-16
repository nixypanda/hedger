module Ui.Button exposing
    ( ButtonConfig
    , ButtonTone(..)
    , ButtonWidth(..)
    , IconButtonConfig
    , SubmitState(..)
    , iconButtonWithTone
    , refreshButton
    , submittingOrIdle
    , textButton
    , textButtonWithAttributes
    , unavailableButton
    , unavailableIconButton
    )

{-| Buttons share one implementation with three axes:

  - `ButtonTone` chooses the semantic colour pairing.
  - `ButtonWidth` chooses how the button claims horizontal space.
  - `SubmitState` optionally makes the button busy or disabled.

Action buttons are created with `textButton` / `textButtonWithAttributes` (for
labelled buttons) or `iconButtonWithTone` (for icon-only buttons). Specialized
refresh and unavailable helpers remain as dedicated entry points.

`FullWidth` is for a button that owns a column. Do not combine it with sibling
buttons in one row: every fill child would claim the full row and overflow.
Multi-action rows (for example the query panel) use `ContentWidth`.

-}

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
        ( borderHairline
        , minimumTouchTarget
        , radiusControl
        , radiusPill
        , space4
        )
import Ui.Icon as Icon exposing (Icon)
import Ui.Motion as Motion
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography


type SubmitState
    = Idle
    | Submitting
    | Disabled


type ButtonTone
    = Primary
    | Danger
    | Ghost
    | GhostFilled


type ButtonWidth
    = ContentWidth
    | FullWidth


type alias ButtonConfig msg =
    { tone : ButtonTone
    , width : ButtonWidth
    , label : String
    , onPress : msg
    , submitState : Maybe SubmitState
    , submittingLabel : Maybe String
    , trailing : Maybe (Element msg)
    }


type alias IconButtonConfig msg =
    { icon : Icon
    , title : String
    , onPress : Maybe msg
    , tone : ButtonTone
    }


submittingOrIdle : Bool -> SubmitState
submittingOrIdle isSubmitting =
    if isSubmitting then
        Submitting

    else
        Idle


textButton : Theme -> ButtonConfig msg -> Element msg
textButton theme config =
    textButtonWithAttributes theme [] config


{-| Same as `textButton`, with caller-supplied attributes on the actual button.
Use this for identity and accessibility attributes, such as an `id`, `aria-label`,
or `aria-describedby`, that depend on context the visible label omits.

`Element.Input.button` installs its press handlers before this list, while the
component's tone, width, `aria-busy`, `aria-disabled`, and disabled state are
applied after it, so those state-critical attributes win. Elm attributes are
opaque, though, so an interaction attribute in `extraAttrs` (for example
`Html.Events.onClick`) still replaces the button's press handler. A caller
`style` can also override an inline tone colour because CSS gives inline styles
higher specificity. Do not pass interaction handlers or styles here.

-}
textButtonWithAttributes :
    Theme
    -> List (Element.Attribute msg)
    -> ButtonConfig msg
    -> Element msg
textButtonWithAttributes theme extraAttrs config =
    let
        state =
            config.submitState

        isActive =
            isActiveState state

        label =
            case ( state, config.submittingLabel ) of
                ( Just Submitting, Just submittingLabel ) ->
                    submittingLabel

                _ ->
                    config.label
    in
    textButtonView theme
        { tone = config.tone
        , width = config.width
        , label = buttonLabel label config.trailing
        , onPress =
            if isActive then
                Just config.onPress

            else
                Nothing
        , submitState = state
        , extraAttrs = extraAttrs
        }


{-| A muted secondary action that still receives the click so the page can
replay its refusal nudge.
-}
unavailableButton :
    Theme
    -> { label : String, reason : String, nudge : Maybe Int, onPress : msg }
    -> Element msg
unavailableButton theme config =
    Input.button
        ([ Element.paddingEach { top = 11, right = 16, bottom = 11, left = 16 }
         , Border.rounded radiusControl
         , Border.width borderHairline
         , Border.color theme.divider
         , Background.color theme.cardBg
         , Font.color theme.textMuted
         , Font.center
         , Common.uiClass "ui-motion ui-button ui-button-ghost"
         , Motion.nudge config.nudge
         , Element.htmlAttribute (Html.Attributes.title config.reason)
         , Element.htmlAttribute
            (Html.Attributes.attribute "aria-label"
                (config.label ++ " (" ++ config.reason ++ ")")
            )
         ]
            ++ Typography.label
            ++ Motion.disabled
            ++ [ Element.height (Element.shrink |> Element.minimum minimumTouchTarget) ]
        )
        { onPress = Just config.onPress
        , label = Element.el [ Element.centerX ] (Element.text config.label)
        }


{-| An icon-only button that keeps the native title and the accessible name in
sync. `Nothing` renders the disabled visual and interaction state.
-}
iconButtonWithTone : Theme -> IconButtonConfig msg -> Element msg
iconButtonWithTone theme config =
    let
        isActive =
            config.onPress /= Nothing

        colors =
            if isActive then
                tonedColors theme config.tone

            else
                disabledColors theme

        hoverClass =
            if isActive then
                " " ++ iconHoverClass config.tone

            else
                ""

        borderHoverClass =
            if isActive && config.tone == Ghost then
                " ui-hover-border-primary"

            else
                ""

        disabledAttrs =
            if isActive then
                []

            else
                Motion.disabled
    in
    Input.button
        ([ Element.width (Element.px minimumTouchTarget)
         , Element.height (Element.px minimumTouchTarget)
         , Background.color colors.background
         , Font.color colors.foreground
         , Font.center
         , Border.rounded radiusPill
         , Border.width borderHairline
         , Border.color colors.border
         , Common.uiClass
            ("ui-motion ui-button "
                ++ toneClass config.tone
                ++ hoverClass
                ++ borderHoverClass
            )
         , Motion.pressable
         , Element.htmlAttribute (Html.Attributes.title config.title)
         , Region.description config.title
         ]
            ++ disabledAttrs
        )
        { onPress = config.onPress
        , label =
            Icon.view
                { icon = config.icon
                , size = 20
                , color = colors.foreground
                }
        }


{-| A muted icon action that still receives the click so the page can replay its
refusal nudge. The reason is used as the native title; the accessible name
combines the icon title with the reason.
-}
unavailableIconButton :
    Theme
    -> { icon : Icon, title : String, reason : String, nudge : Maybe Int, onPress : msg }
    -> Element msg
unavailableIconButton theme config =
    Input.button
        ([ Element.width (Element.px minimumTouchTarget)
         , Element.height (Element.px minimumTouchTarget)
         , Background.color theme.cardBg
         , Font.color theme.textMuted
         , Font.center
         , Border.rounded radiusPill
         , Border.width borderHairline
         , Border.color theme.divider
         , Common.uiClass "ui-motion ui-button ui-button-ghost"
         , Motion.nudge config.nudge
         , Element.htmlAttribute (Html.Attributes.title config.reason)
         , Element.htmlAttribute
            (Html.Attributes.attribute "aria-label"
                (config.title ++ " (" ++ config.reason ++ ")")
            )
         ]
            ++ Motion.disabled
        )
        { onPress = Just config.onPress
        , label =
            Icon.view
                { icon = config.icon
                , size = 20
                , color = theme.textMuted
                }
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
         , Border.width borderHairline
         , Border.color theme.divider
         , Common.uiClass
            ("ui-motion ui-button ui-button-ghost "
                ++ "ui-hover-primary-soft ui-hover-border-primary"
            )
         , Motion.pressable
         , Element.htmlAttribute (Html.Attributes.title title)
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
                    Element.htmlAttribute (Html.Attributes.attribute "inert" "")
                        :: Motion.busy

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


type alias TextButtonView msg =
    { tone : ButtonTone
    , width : ButtonWidth
    , label : Element msg
    , onPress : Maybe msg
    , submitState : Maybe SubmitState
    , extraAttrs : List (Element.Attribute msg)
    }


textButtonView : Theme -> TextButtonView msg -> Element msg
textButtonView theme config =
    let
        isActive =
            isActiveState config.submitState

        colors =
            if isActive then
                tonedColors theme config.tone

            else
                disabledColors theme

        hoverClass =
            if isActive then
                " " ++ textHoverClass config.tone

            else
                ""

        stateAttrs =
            stateAttributes config.submitState
    in
    Input.button
        (config.extraAttrs
            ++ [ Element.paddingEach { top = 11, right = 16, bottom = 11, left = 16 }
               , Border.rounded radiusControl
               , Border.width borderHairline
               , Border.color colors.border
               , Background.color colors.background
               , Font.color colors.foreground
               , Font.center
               , Common.uiClass
                    ("ui-motion ui-button "
                        ++ toneClass config.tone
                        ++ hoverClass
                    )
               , Motion.pressable
               ]
            ++ Typography.label
            ++ widthAttributes config.width
            ++ stateAttrs
            ++ [ Element.height (Element.shrink |> Element.minimum minimumTouchTarget)
               , Element.htmlAttribute (Html.Attributes.disabled (not isActive))
               ]
        )
        { onPress = config.onPress
        , label = config.label
        }


buttonLabel : String -> Maybe (Element msg) -> Element msg
buttonLabel label trailing =
    case trailing of
        Nothing ->
            Element.el [ Element.centerX ] (Element.text label)

        Just trailingElement ->
            Element.row
                [ Element.width Element.fill
                , Element.spacing space4
                ]
                [ Element.el [ Element.centerY ] (Element.text label)
                , Element.el [ Element.alignRight, Element.centerY ] trailingElement
                ]


type alias ToneColors =
    { background : Element.Color
    , foreground : Element.Color
    , border : Element.Color
    }


{-| Enabled colours are shared by text and icon buttons; their hover classes
differ below because an icon button has no label to carry the affordance.
-}
tonedColors : Theme -> ButtonTone -> ToneColors
tonedColors theme tone =
    case tone of
        Primary ->
            { background = theme.primary
            , foreground = theme.primaryButtonFg
            , border = theme.primary
            }

        Danger ->
            { background = theme.dangerSolid
            , foreground = theme.dangerSolidFg
            , border = theme.dangerSolid
            }

        Ghost ->
            { background = theme.cardBg
            , foreground = theme.textPrimary
            , border = theme.divider
            }

        GhostFilled ->
            { background = theme.primarySoft
            , foreground = theme.primary
            , border = theme.primarySoft
            }


{-| A disabled control uses the same muted surface in both themes. `textMuted`
on `inputFill` stays above a 4.5:1 contrast ratio in light and dark mode, unlike
the tone colours, where a white label on `primaryDisabled` falls below it.
-}
disabledColors : Theme -> ToneColors
disabledColors theme =
    { background = theme.inputFill
    , foreground = theme.textMuted
    , border = theme.divider
    }


textHoverClass : ButtonTone -> String
textHoverClass tone =
    case tone of
        Primary ->
            "ui-hover-primary"

        Danger ->
            "ui-hover-danger"

        Ghost ->
            "ui-hover-surface"

        GhostFilled ->
            "ui-hover-selected"


iconHoverClass : ButtonTone -> String
iconHoverClass tone =
    case tone of
        Primary ->
            "ui-hover-primary"

        Danger ->
            "ui-hover-danger"

        Ghost ->
            "ui-hover-primary-soft"

        GhostFilled ->
            "ui-hover-selected"


isActiveState : Maybe SubmitState -> Bool
isActiveState state =
    case state of
        Nothing ->
            True

        Just Idle ->
            True

        Just Submitting ->
            False

        Just Disabled ->
            False


{-| Internal `aria-*` state is always present so a caller attribute cannot make
an enabled button look disabled or a submitting button look idle.
-}
stateAttributes : Maybe SubmitState -> List (Element.Attribute msg)
stateAttributes state =
    case state of
        Just Submitting ->
            Motion.busy ++ Motion.disabled

        Just Disabled ->
            Motion.disabled ++ [ ariaAttribute "aria-busy" "false" ]

        Nothing ->
            idleAttributes

        Just Idle ->
            idleAttributes


idleAttributes : List (Element.Attribute msg)
idleAttributes =
    [ ariaAttribute "aria-busy" "false"
    , ariaAttribute "aria-disabled" "false"
    ]


ariaAttribute : String -> String -> Element.Attribute msg
ariaAttribute name value =
    Element.htmlAttribute (Html.Attributes.attribute name value)


widthAttributes : ButtonWidth -> List (Element.Attribute msg)
widthAttributes width =
    case width of
        ContentWidth ->
            [ Element.width Element.shrink ]

        FullWidth ->
            [ Element.width Element.fill ]


toneClass : ButtonTone -> String
toneClass tone =
    case tone of
        Primary ->
            "ui-button-primary"

        Danger ->
            "ui-button-danger"

        Ghost ->
            "ui-button-ghost"

        GhostFilled ->
            "ui-button-ghost-filled"
