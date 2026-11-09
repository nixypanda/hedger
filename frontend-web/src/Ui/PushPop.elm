module Ui.PushPop exposing
    ( Pane(..)
    , Transition
    , arrivedAt
    , close
    , current
    , init
    , isRunning
    , open
    , outgoing
    , target
    , update
    , view
    )

{-| A list/detail push-pop timeline for compact layouts.

The current pane is the stable base. While the timeline runs, the previous pane
is rendered in front as an inert overlay, so dropping it at the end of the
transition never moves the remaining pane into a different DOM slot. The
overlay is a sibling of the animated base, never a descendant, so its opacity
and transform do not compound with the base's.

Only opacity and horizontal transforms are animated. The travel distance is the
viewport width, and reduced motion snaps straight to the requested pane.

-}

import Element exposing (Element)
import Element.Background as Background
import Html.Attributes
import Time
import Ui.Animation as Animation
import Ui.Common as Common
import Ui.Env exposing (Env)


type Pane
    = ListPane
    | DetailPane


type Transition
    = Transition Details


type alias Details =
    { animation : Animation.Transition Pane
    , target : Pane
    , outgoing : Maybe Pane
    , distance : Float
    }


duration : Animation.Duration
duration =
    220


init : Pane -> Transition
init pane =
    Transition
        { animation = Animation.init pane
        , target = pane
        , outgoing = Nothing
        , distance = 0
        }


open : { reducedMotion : Bool, distance : Float } -> Transition -> Transition
open config (Transition transition) =
    let
        currentPane =
            Animation.current transition.animation

        running =
            Animation.isRunning transition.animation
    in
    case ( transition.target, currentPane, running ) of
        ( DetailPane, _, _ ) ->
            Transition transition

        ( ListPane, _, True ) ->
            snap config DetailPane transition

        ( ListPane, _, False ) ->
            startOpen config transition


snap : { reducedMotion : Bool, distance : Float } -> Pane -> Details -> Transition
snap config pane transition =
    Transition
        { transition
            | animation = Animation.init pane
            , target = pane
            , outgoing = Nothing
            , distance = config.distance
        }


close : { reducedMotion : Bool, distance : Float } -> Transition -> Transition
close config (Transition transition) =
    let
        currentPane =
            Animation.current transition.animation

        running =
            Animation.isRunning transition.animation
    in
    case ( transition.target, currentPane, running ) of
        ( ListPane, _, _ ) ->
            Transition transition

        ( DetailPane, _, True ) ->
            snap config ListPane transition

        ( DetailPane, _, False ) ->
            startClose config transition


startOpen : { reducedMotion : Bool, distance : Float } -> Details -> Transition
startOpen config transition =
    let
        animation =
            Animation.to
                { reducedMotion = config.reducedMotion }
                duration
                DetailPane
                transition.animation
    in
    Transition
        { animation = animation
        , target = DetailPane
        , outgoing =
            if Animation.isRunning animation then
                Just ListPane

            else
                Nothing
        , distance = config.distance
        }


startClose : { reducedMotion : Bool, distance : Float } -> Details -> Transition
startClose config transition =
    let
        animation =
            Animation.to
                { reducedMotion = config.reducedMotion }
                duration
                ListPane
                transition.animation
    in
    Transition
        { animation = animation
        , target = ListPane
        , outgoing =
            if Animation.isRunning animation then
                Just DetailPane

            else
                Nothing
        , distance = config.distance
        }


update : Time.Posix -> Transition -> Transition
update now (Transition transition) =
    let
        animation =
            Animation.update now transition.animation
    in
    Transition
        { transition
            | animation = animation
            , outgoing =
                if Animation.isRunning animation then
                    transition.outgoing

                else
                    Nothing
        }


isRunning : Transition -> Bool
isRunning (Transition transition) =
    Animation.isRunning transition.animation


current : Transition -> Pane
current (Transition transition) =
    Animation.current transition.animation


outgoing : Transition -> Maybe Pane
outgoing (Transition transition) =
    transition.outgoing


{-| The pane the latest open or close requested. This is available before the
timeline receives its first tick, unlike `current`.
-}
target : Transition -> Pane
target (Transition transition) =
    transition.target


arrivedAt : Pane -> Time.Posix -> Transition -> Bool
arrivedAt pane now (Transition transition) =
    Animation.arrivedAt pane now transition.animation


{-| Render the current pane and, while the timeline runs, the outgoing pane in
front of it. The outgoing pane is inert and hidden from assistive technology, so
it cannot take focus or receive scroll changes while it leaves.
-}
view : Env -> Transition -> Element msg -> Maybe (Element msg) -> Element msg
view env (Transition transition) listPane detailPane =
    let
        distance =
            transition.distance

        leaving =
            if Animation.isRunning transition.animation then
                transition.outgoing

            else
                Nothing

        basePane =
            case ( transition.target, detailPane ) of
                ( DetailPane, Nothing ) ->
                    ListPane

                _ ->
                    transition.target

        baseContent =
            case ( basePane, detailPane ) of
                ( DetailPane, Just detail ) ->
                    detail

                _ ->
                    listPane

        outgoingElement =
            case leaving of
                Just pane ->
                    if pane == basePane then
                        Nothing

                    else
                        case ( pane, detailPane ) of
                            ( DetailPane, Just detail ) ->
                                Just
                                    (paneElement env
                                        transition.animation
                                        (anchoredVisual pane distance)
                                        outgoingAttributes
                                        detail
                                    )

                            ( DetailPane, Nothing ) ->
                                Nothing

                            ( ListPane, _ ) ->
                                Just
                                    (paneElement env
                                        transition.animation
                                        (anchoredVisual pane distance)
                                        outgoingAttributes
                                        listPane
                                    )

                Nothing ->
                    Nothing

        layer =
            case outgoingElement of
                Just element ->
                    [ Element.inFront element ]

                Nothing ->
                    []
    in
    Element.el
        ([ Element.width Element.fill
         , Element.height Element.fill
         , Element.htmlAttribute (Html.Attributes.style "position" "relative")
         ]
            ++ layer
        )
        (Animation.view
            { attributes =
                [ Element.width Element.fill
                , Element.height Element.fill
                , Common.minWidthZero
                ]
            , transition = transition.animation
            , visual = anchoredVisual basePane distance
            , content = baseContent
            }
        )


paneElement :
    Env
    -> Animation.Transition Pane
    -> (Pane -> Animation.Visual)
    -> List (Element.Attribute msg)
    -> Element msg
    -> Element msg
paneElement env transition visual attributes content =
    Animation.view
        { attributes =
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.clip
            , Background.color env.theme.pageBg
            , Common.minWidthZero
            ]
                ++ attributes
        , transition = transition
        , visual = visual
        , content = content
        }


outgoingAttributes : List (Element.Attribute msg)
outgoingAttributes =
    [ Common.inert, Common.ariaHidden ]


{-| Anchor the pane that should sit at rest; the other pane is placed one
viewport width away. The anchor decides the direction: a list anchor enters
from the left, a detail anchor enters from the right.
-}
anchoredVisual : Pane -> Float -> Pane -> Animation.Visual
anchoredVisual anchor distance pane =
    if pane == anchor then
        Animation.Slide { opacity = 1, x = 0, y = 0 }

    else
        case anchor of
            ListPane ->
                Animation.Slide { opacity = 0, x = -distance, y = 0 }

            DetailPane ->
                Animation.Slide { opacity = 0, x = distance, y = 0 }
