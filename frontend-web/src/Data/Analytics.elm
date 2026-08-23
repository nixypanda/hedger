module Data.Analytics exposing
    ( ActivityBucket
    , ActivityDirection(..)
    , BreakdownItem
    , DirectionBreakdown
    , Kpi
    , TimelinePoint
    )

import Data.AccountType exposing (AccountType)
import Data.Completeness exposing (Completeness)
import Data.Date exposing (Date)
import Data.Decimal exposing (Decimal)
import Data.Transaction exposing (Transaction)


type alias BreakdownItem =
    { account : String
    , accountType : Maybe AccountType
    , quantity : Decimal
    }


type ActivityDirection
    = Increase
    | Reallocate
    | Decrease


type alias DirectionBreakdown =
    { direction : ActivityDirection
    , categories : List BreakdownItem
    }


type alias ActivityBucket =
    { key : String
    , startDate : Date
    , total : Decimal
    , increase : Decimal
    , decrease : Decimal
    , reallocated : Decimal
    , categories : List BreakdownItem
    , directions : List DirectionBreakdown
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
    , completeness : Completeness
    , missingCommodities : List String
    }
