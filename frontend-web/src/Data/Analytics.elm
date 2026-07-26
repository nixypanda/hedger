module Data.Analytics exposing (Kpi, TimelinePoint)

import Data.Date exposing (Date)
import Data.Decimal exposing (Decimal)


type alias Kpi =
    { value : Decimal
    , absoluteChange : Maybe Decimal
    , percentageChange : Maybe Decimal
    , trend : List Decimal
    }


type alias TimelinePoint =
    { period : String
    , startDate : Date
    , positive : Decimal
    , negative : Decimal
    , net : Decimal
    }
