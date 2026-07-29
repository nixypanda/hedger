module Ui.Responsive exposing
    ( ContentLayout(..)
    , contentLayout
    , contentPadding
    , isCompact
    , usableContentWidth
    , usesBottomNavigation
    , usesMasterDetail
    )

import State.Viewport exposing (Viewport)


type ContentLayout
    = Compact
    | Medium
    | Wide


usesBottomNavigation : Viewport -> Bool
usesBottomNavigation viewport =
    viewport.width < 1544


contentPadding : Int
contentPadding =
    16


usableContentWidth : Viewport -> Int
usableContentWidth viewport =
    let
        navigationWidth =
            if usesBottomNavigation viewport then
                0

            else
                232
    in
    min 1280 (max 0 (viewport.width - navigationWidth - 2 * contentPadding))


contentLayout : Viewport -> ContentLayout
contentLayout viewport =
    let
        availableWidth =
            usableContentWidth viewport
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


usesMasterDetail : Viewport -> Bool
usesMasterDetail viewport =
    contentLayout viewport == Wide
