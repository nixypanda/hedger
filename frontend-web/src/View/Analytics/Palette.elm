module View.Analytics.Palette exposing
    ( activityCategoryColor
    , activityColor
    , directionSpecsFor
    , flowMovementColors
    , otherColor
    , paletteColor
    , scaleColor
    )

import Data.Analytics exposing (ActivityDirection(..))
import Element
import Ui.Theme as Theme exposing (Theme)
import View.Analytics.Types exposing (ActivityTone(..), DirectionSpec, Palette(..))


paletteColor : Theme -> Palette -> Int -> Element.Color
paletteColor theme palette index =
    let
        order =
            case palette of
                PositivePalette ->
                    [ 1, 7, 4, 3, 0, 2, 9, 5, 8 ]

                AssetPalette ->
                    [ 1, 7, 4, 3, 0, 2, 9, 5, 8 ]

                NegativePalette ->
                    [ 5, 8, 2, 0, 3, 4, 9, 1, 7 ]

                LiabilityPalette ->
                    [ 5, 8, 2, 0, 3, 4, 9, 1, 7 ]

        colorIndex =
            order |> List.drop (modBy (List.length order) index) |> List.head |> Maybe.withDefault 0
    in
    theme.chartSeries |> List.drop colorIndex |> List.head |> Maybe.withDefault theme.primary


otherColor : Theme -> Element.Color
otherColor theme =
    theme.chartSeries |> List.drop 6 |> List.head |> Maybe.withDefault theme.primary


activityColor : Theme -> ActivityTone -> Element.Color
activityColor theme tone =
    case tone of
        IncomeTone ->
            theme.income

        ExpenseTone ->
            theme.liability

        AssetTone ->
            theme.positive

        LiabilityTone ->
            theme.negative


activityCategoryColor : Theme -> ActivityTone -> Int -> Element.Color
activityCategoryColor theme tone index =
    paletteColor theme
        (case tone of
            IncomeTone ->
                PositivePalette

            ExpenseTone ->
                NegativePalette

            AssetTone ->
                AssetPalette

            LiabilityTone ->
                LiabilityPalette
        )
        index


flowMovementColors : Theme -> ActivityTone -> ( Element.Color, Element.Color )
flowMovementColors theme tone =
    case tone of
        ExpenseTone ->
            ( theme.negative, theme.positive )

        _ ->
            ( theme.positive, theme.negative )


directionSpecsFor : Theme -> ActivityTone -> Maybe (List DirectionSpec)
directionSpecsFor theme tone =
    case tone of
        AssetTone ->
            Just
                [ { direction = Increase, label = "Acquire", scale = theme.acquireScale }
                , { direction = Reallocate, label = "Transfer", scale = theme.rebalanceScale }
                , { direction = Decrease, label = "Dispose", scale = theme.disposeScale }
                ]

        LiabilityTone ->
            Nothing

        _ ->
            Nothing


scaleColor : List Element.Color -> Int -> Element.Color
scaleColor scale index =
    case scale of
        [] ->
            Theme.transparentColor

        _ ->
            scale
                |> List.drop (modBy (List.length scale) index)
                |> List.head
                |> Maybe.withDefault Theme.transparentColor
