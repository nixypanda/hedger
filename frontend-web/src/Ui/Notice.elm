module Ui.Notice exposing
    ( Config
    , Placement(..)
    , Tone(..)
    , view
    )

{-| A transient status or error message rendered in a fixed-position toast.

Placement is inline-styled on the toast element so elm-ui's generated layout
classes cannot put it back into page flow. PWA install/update prompts use the
top edge; page and query errors use the bottom edge above the bottom navigation.
Action prompts capture the whole toast, while plain messages pass clicks through
to the page underneath.

-}

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes as Attr
import Ui.Common as Common
import Ui.DesignTokens as Tokens
import Ui.Icon as Icon exposing (Icon)
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography


type Tone
    = Info
    | Danger


type Placement
    = Top
    | Bottom


type alias Config msg =
    { placement : Placement
    , tone : Tone
    , icon : Icon
    , message : String
    , actions : List (Element msg)
    , id : Maybe String
    }


view : Theme -> Config msg -> Element msg
view theme config =
    let
        colors =
            toneColors theme config.tone

        hasActions =
            not (List.isEmpty config.actions)
    in
    Element.row
        ([ Element.spacing Tokens.space12
         , Element.padding Tokens.space16
         , Background.color colors.background
         , Border.rounded Tokens.radiusCard
         , Border.width Tokens.borderHairline
         , Border.color colors.border
         , Font.color colors.foreground
         , Common.uiClass (placementClass config.placement)
         , Element.htmlAttribute (Attr.style "position" "fixed")
         , Element.htmlAttribute (Attr.style "left" "50%")
         , Element.htmlAttribute (Attr.style "right" "auto")
         , Element.htmlAttribute (Attr.style "top" (placementTop config.placement))
         , Element.htmlAttribute (Attr.style "bottom" (placementBottom config.placement))
         , Element.htmlAttribute (Attr.style "transform" "translateX(-50%)")
         , Element.htmlAttribute (Attr.style "width" "min(560px, calc(100vw - 32px))")
         , Element.htmlAttribute (Attr.style "max-width" "calc(100vw - 32px)")
         , Element.htmlAttribute (Attr.style "z-index" "1000")
         , Element.htmlAttribute
            (Attr.style "pointer-events"
                (if hasActions then
                    "auto"

                 else
                    "none"
                )
            )
         ]
            ++ roleAttributes config
            ++ (case config.id of
                    Just id ->
                        [ Element.htmlAttribute (Attr.id id) ]

                    Nothing ->
                        []
               )
        )
        [ Icon.view
            { icon = config.icon
            , size = Tokens.iconSizeSmall
            , color = colors.foreground
            }
        , Element.column
            [ Element.width Element.fill
            , Element.spacing Tokens.space8
            ]
            (Element.paragraph
                (Typography.body ++ messageAttributes hasActions)
                [ Element.text config.message ]
                :: (if hasActions then
                        [ Element.wrappedRow
                            [ Element.width Element.fill
                            , Element.spacing Tokens.space8
                            ]
                            config.actions
                        ]

                    else
                        []
                   )
            )
        ]


messageAttributes : Bool -> List (Element.Attribute msg)
messageAttributes hasActions =
    if hasActions then
        [ Element.width Element.fill
        , Element.htmlAttribute (Attr.attribute "role" "status")
        , Element.htmlAttribute (Attr.attribute "aria-live" "polite")
        ]

    else
        [ Element.width Element.fill ]


roleAttributes : Config msg -> List (Element.Attribute msg)
roleAttributes config =
    if List.isEmpty config.actions then
        [ Element.htmlAttribute
            (Attr.attribute "role"
                (if config.tone == Danger then
                    "alert"

                 else
                    "status"
                )
            )
        , Element.htmlAttribute
            (Attr.attribute "aria-live"
                (if config.tone == Danger then
                    "assertive"

                 else
                    "polite"
                )
            )
        ]

    else
        [ Element.htmlAttribute (Attr.attribute "role" "region")
        , Element.htmlAttribute (Attr.attribute "aria-label" config.message)
        ]


placementClass : Placement -> String
placementClass placement =
    case placement of
        Top ->
            "ui-notice ui-notice-top"

        Bottom ->
            "ui-notice ui-notice-bottom"


placementTop : Placement -> String
placementTop placement =
    case placement of
        Top ->
            "calc(env(safe-area-inset-top, 0px) + 16px)"

        Bottom ->
            "auto"


placementBottom : Placement -> String
placementBottom placement =
    case placement of
        Top ->
            "auto"

        Bottom ->
            "calc(env(safe-area-inset-bottom, 0px) + 24px)"


type alias ToneColors =
    { background : Element.Color
    , border : Element.Color
    , foreground : Element.Color
    }


toneColors : Theme -> Tone -> ToneColors
toneColors theme tone =
    case tone of
        Info ->
            { background = theme.primarySoft
            , border = theme.primary
            , foreground = theme.primary
            }

        Danger ->
            { background = theme.dangerBg
            , border = theme.dangerSolid
            , foreground = theme.dangerFg
            }
