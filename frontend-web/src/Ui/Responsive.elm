module Ui.Responsive exposing
    ( ContentLayout(..)
    , contentLayout
    , contentPadding
    , isCompact
    , usableContentWidth
    , usesBottomNavigation
    )

import Element exposing (Device)
import State.Viewport exposing (Viewport)


type ContentLayout
    = Compact
    | Medium
    | Wide


usesBottomNavigation : Viewport -> Bool
usesBottomNavigation viewport =
    viewport.width < 1200


contentPadding : Device -> Int
contentPadding device =
    if device.class == Element.Phone then
        16

    else
        24


usableContentWidth : Viewport -> Device -> Int
usableContentWidth viewport device =
    let
        navigationWidth =
            if usesBottomNavigation viewport then
                0

            else
                232
    in
    max 0 (viewport.width - navigationWidth - 2 * contentPadding device)


contentLayout : Viewport -> Device -> ContentLayout
contentLayout viewport device =
    let
        availableWidth =
            usableContentWidth viewport device
    in
    if availableWidth < 700 then
        Compact

    else if availableWidth < 960 then
        Medium

    else
        Wide


isCompact : ContentLayout -> Bool
isCompact layout =
    layout == Compact
