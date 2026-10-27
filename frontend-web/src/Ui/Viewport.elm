module Ui.Viewport exposing
    ( ContentLayout(..)
    , Viewport
    , activityCalendarColumns
    , contentLayout
    , contentPadding
    , isCompact
    , usableContentWidth
    , usesBottomNavigation
    , usesMasterDetail
    , usesNavigationLabels
    , usesTwoColumnCards
    )

import Ui.DesignTokens as DesignTokens


type alias Viewport =
    { width : Int
    , height : Int
    }


type ContentLayout
    = Compact
    | Medium
    | Wide


usesBottomNavigation : Viewport -> Bool
usesBottomNavigation viewport =
    viewport.width < 1544


contentPadding : Int
contentPadding =
    DesignTokens.space16


usableContentWidth : Viewport -> Int
usableContentWidth viewport =
    let
        navigationWidth =
            if usesBottomNavigation viewport then
                0

            else
                DesignTokens.sidebarWidth
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


{-| Bottom navigation keeps its text labels while each tab is wide enough for
the longest destination name. Below this width the labels are dropped and the
bar becomes icon-only.
-}
usesNavigationLabels : Viewport -> Bool
usesNavigationLabels viewport =
    viewport.width >= navigationLabelWidth


{-| Two-column analysis cards only start once each card has enough room for a
donut or calendar next to its legend. Below this the cards stack full-width.
-}
usesTwoColumnCards : Viewport -> Bool
usesTwoColumnCards viewport =
    usableContentWidth viewport >= twoColumnCardWidth


{-| How many yearly activity rings fit across one analysis card. The card may
be full-width or half of a two-column grid, so use its real inner width rather
than the page width.
-}
activityCalendarColumns : Viewport -> Int
activityCalendarColumns viewport =
    let
        cardWidth =
            if usesTwoColumnCards viewport then
                (usableContentWidth viewport - DesignTokens.space16) // 2

            else
                usableContentWidth viewport

        innerWidth =
            cardWidth
                - 2
                * DesignTokens.space20
                - 2
                * DesignTokens.borderHairline
    in
    if innerWidth < 360 then
        3

    else if innerWidth < 540 then
        4

    else
        6


navigationLabelWidth : Int
navigationLabelWidth =
    600


twoColumnCardWidth : Int
twoColumnCardWidth =
    1200
