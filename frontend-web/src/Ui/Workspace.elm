module Ui.Workspace exposing (Pane, PaneWidth(..), view)

{-| Splits a page into independently scrolling panes.

The caller decides which panes are visible for the current layout. This module
only lays them out and owns their scroll containers; it never filters or guesses
an active pane.

A pane dropped from the list is unmounted, so it does not retain its scroll
offset when it is shown again.

-}

import Element exposing (Element)
import Element.Background as Background
import Element.Keyed as Keyed
import Html.Attributes
import Ui.Common as Common
import Ui.Env exposing (Env)
import Ui.Theme exposing (Theme)


type PaneWidth
    = Fill
    | Portion Int
    | Fixed Int


type alias Pane msg =
    { key : String
    , width : PaneWidth
    , content : Element msg
    }


view : Env -> List (Pane msg) -> Element msg
view env panes =
    case panes of
        [] ->
            Element.none

        [ pane ] ->
            pane.content

        _ ->
            Keyed.row
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.clip
                , minHeightZero
                , flexShrinkOne
                , Background.color env.theme.pageBg
                ]
                (List.map (paneView env.theme) panes)


paneView : Theme -> Pane msg -> ( String, Element msg )
paneView theme pane =
    ( pane.key
    , Element.el
        [ Element.width (widthLength pane.width)
        , Element.height Element.fill
        , Element.scrollbarY
        , overflowXHidden
        , Common.minWidthZero
        , minHeightZero
        , Common.uiClass "ui-scroll-stable"
        , Background.color theme.pageBg
        ]
        pane.content
    )


widthLength : PaneWidth -> Element.Length
widthLength width =
    case width of
        Fill ->
            Element.fill

        Portion portion ->
            Element.fillPortion portion

        Fixed pixels ->
            Element.px pixels


minHeightZero : Element.Attribute msg
minHeightZero =
    Element.htmlAttribute (Html.Attributes.style "min-height" "0")


flexShrinkOne : Element.Attribute msg
flexShrinkOne =
    Element.htmlAttribute (Html.Attributes.style "flex-shrink" "1")


overflowXHidden : Element.Attribute msg
overflowXHidden =
    Element.htmlAttribute (Html.Attributes.style "overflow-x" "hidden")
