module Ui.Animation exposing
    ( Animated
    , Duration
    , Transition
    , Visual(..)
    , arrived
    , arrivedAt
    , current
    , init
    , isRunning
    , previous
    , render
    , to
    , update
    , view
    )

{-| Elm-owned continuity animations.

This is the only module that imports `Animator` and `Animator.Timeline`.
Everything else works with the opaque `Transition` type and the small
`Visual` vocabulary below.

A transition is either snapped or animated. Snapped transitions report their
target immediately and never run, which is how reduced motion and initial
states avoid producing a timeline at all.

-}

import Animator
import Animator.Timeline as Timeline exposing (Timeline)
import Element exposing (Element)
import Html
import Html.Attributes as Attributes
import Time


type Transition state
    = Snapped state
    | Running
        { timeline : Timeline state
        , pending : Maybe (Timeline state)
        }


type alias Duration =
    Int


{-| A resting or moving element's CSS-expressible state.

Use `Fade` when the element already owns a transform, such as the fixed-position
notice toast, and `Slide` for elements the animation may translate.

-}
type Visual
    = Fade Float
    | Slide { opacity : Float, x : Float, y : Float }


type alias Animated msg =
    { attributes : List (Element.Attribute msg)
    , stylesheet : Element msg
    }


init : state -> Transition state
init state =
    Snapped state


to : { reducedMotion : Bool } -> Duration -> state -> Transition state -> Transition state
to config duration next transition =
    if config.reducedMotion then
        Snapped next

    else
        case transition of
            Snapped state ->
                Running
                    { timeline =
                        Timeline.to
                            (Animator.ms (toFloat duration))
                            next
                            (Timeline.init state)
                    , pending = Nothing
                    }

            Running running ->
                Running
                    { timeline =
                        Timeline.to
                            (Animator.ms (toFloat duration))
                            next
                            running.timeline
                    , pending = Just running.timeline
                    }


update : Time.Posix -> Transition state -> Transition state
update now transition =
    case transition of
        Snapped _ ->
            transition

        Running running ->
            Running
                { timeline = Timeline.update now running.timeline
                , pending = Nothing
                }


isRunning : Transition state -> Bool
isRunning transition =
    case transition of
        Snapped _ ->
            False

        Running running ->
            Timeline.isRunning running.timeline


current : Transition state -> state
current transition =
    case transition of
        Snapped state ->
            state

        Running running ->
            Timeline.current running.timeline


previous : Transition state -> state
previous transition =
    case transition of
        Snapped state ->
            state

        Running running ->
            Timeline.previous running.timeline


arrived : Transition state -> state
arrived transition =
    case transition of
        Snapped state ->
            state

        Running running ->
            Timeline.arrived running.timeline


arrivedAt : state -> Time.Posix -> Transition state -> Bool
arrivedAt state now transition =
    case transition of
        Snapped _ ->
            False

        Running running ->
            Timeline.arrivedAt state now running.timeline


{-| Build the attributes and stylesheet for an element's current timeline state.
The stylesheet must be mounted as a child so the generated keyframes exist.

While an interruption waits for its first tick, render the previous timeline so
an already-running animation continues from its sampled state instead of
snapping to rest or re-emitting stale keyframes. A transition started from rest
has no pending timeline and renders at rest for that frame.

-}
render : Transition state -> (state -> Visual) -> Animated msg
render transition toVisual =
    case transition of
        Snapped _ ->
            resting

        Running running ->
            renderTimeline
                toVisual
                (Maybe.withDefault running.timeline running.pending)


renderTimeline : (state -> Visual) -> Timeline state -> Animated msg
renderTimeline toVisual timeline =
    if Timeline.isRunning timeline && not (Timeline.hasChanges timeline) then
        let
            css =
                Animator.toCss
                    (Animator.onTimeline timeline
                        (\state -> visualAttributes (toVisual state))
                    )
        in
        { attributes = List.map styleAttribute css.props
        , stylesheet = stylesheet css.keyframes
        }

    else
        resting


resting : Animated msg
resting =
    { attributes = [], stylesheet = stylesheet "" }


visualAttributes : Visual -> List Animator.Attribute
visualAttributes visual =
    case visual of
        Fade opacity ->
            [ Animator.opacity opacity ]

        Slide config ->
            [ Animator.opacity config.opacity
            , Animator.x config.x
            , Animator.y config.y
            ]


styleAttribute : ( String, String ) -> Element.Attribute msg
styleAttribute ( name, value ) =
    Element.htmlAttribute (Attributes.style name value)


stylesheet : String -> Element msg
stylesheet source =
    Element.html
        (Html.node "style"
            [ Attributes.style "display" "none" ]
            [ Html.text source ]
        )


{-| Wrap content in a styled column. Callers pass layout attributes such as
width and height; the wrapper keeps the generated stylesheet in the same tree.
-}
view :
    { attributes : List (Element.Attribute msg)
    , transition : Transition state
    , visual : state -> Visual
    , content : Element msg
    }
    -> Element msg
view config =
    let
        animated =
            render config.transition config.visual
    in
    Element.column
        (config.attributes ++ animated.attributes)
        [ animated.stylesheet, config.content ]
