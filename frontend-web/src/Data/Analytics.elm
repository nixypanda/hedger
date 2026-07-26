module Data.Analytics exposing (ActivityBucket, BreakdownItem, Kpi, TimelinePoint)

import Data.Date exposing (Date)
import Data.Decimal exposing (Decimal)
import Data.Transaction exposing (Transaction)


type alias BreakdownItem =
    { account : String
    , quantity : Decimal
    }


type alias ActivityBucket =
    { key : String
    , startDate : Date
    , total : Decimal
    , increase : Decimal
    , decrease : Decimal
    , reallocated : Decimal
    , categories : List BreakdownItem
    , transactions : List Transaction
    }


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
