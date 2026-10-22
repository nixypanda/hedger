module View.Analytics.Types exposing
    ( ActivityTone(..)
    , CalendarDisplay(..)
    , DirectionSpec
    , LegendState(..)
    , Palette(..)
    )

import Data.Analytics exposing (ActivityDirection)
import Element


type Palette
    = PositivePalette
    | NegativePalette
    | AssetPalette
    | LiabilityPalette


type ActivityTone
    = IncomeTone
    | ExpenseTone
    | AssetTone
    | LiabilityTone


type CalendarDisplay
    = ByMovement
    | ByCategory


type LegendState
    = LegendCollapsed
    | LegendExpanded


type alias DirectionSpec =
    { direction : ActivityDirection
    , label : String
    , scale : List Element.Color
    }
