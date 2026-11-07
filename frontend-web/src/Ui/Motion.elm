module Ui.Motion exposing
    ( busy
    , chevron
    , disabled
    , flip
    , nudge
    , pressable
    , reveal
    , tab
    )

{-| CSS-driven interaction affordances backed by the motion tokens in
`main.css`. All helpers degrade to their final state under
`prefers-reduced-motion`.

Motion state lives in `data-*` attributes, keeping it distinct from styling
classes and their cascades.

-}

import Element exposing (Attribute)
import Html.Attributes


pressable : Attribute msg
pressable =
    htmlAttribute "data-ui-press" ""


{-| Busy data hook plus `aria-busy`; this does not prevent the action.
-}
busy : List (Attribute msg)
busy =
    [ htmlAttribute "data-ui-busy" ""
    , htmlAttribute "aria-busy" "true"
    ]


{-| Disabled visuals plus `aria-disabled`; this does not prevent the action.
-}
disabled : List (Attribute msg)
disabled =
    [ htmlAttribute "data-ui-disabled" ""
    , htmlAttribute "aria-disabled" "true"
    ]


{-| Attach `Just attempt` only when the interaction is refused; the counter must
alternate parity on each attempt so the nudge replays. `Nothing` keeps the hook
idle.
-}
nudge : Maybe Int -> Attribute msg
nudge attempt =
    htmlAttribute "data-ui-nudge"
        (case attempt of
            Just count ->
                if modBy 2 count == 0 then
                    "a"

                else
                    "b"

            Nothing ->
                "none"
        )


reveal : Attribute msg
reveal =
    htmlAttribute "data-ui-reveal" ""


chevron : Bool -> Attribute msg
chevron expanded =
    htmlAttribute "data-ui-chevron"
        (if expanded then
            "expanded"

         else
            "closed"
        )


flip : Bool -> Attribute msg
flip flipped =
    htmlAttribute "data-ui-flip"
        (if flipped then
            "flipped"

         else
            "closed"
        )


tab : Bool -> Attribute msg
tab active =
    htmlAttribute "data-ui-tab"
        (if active then
            "active"

         else
            "inactive"
        )


htmlAttribute : String -> String -> Attribute msg
htmlAttribute name value =
    Element.htmlAttribute (Html.Attributes.attribute name value)
