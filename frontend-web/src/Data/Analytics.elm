module Data.Analytics exposing
    ( ActivityBucket
    , ActivityDirection(..)
    , ActivityKind(..)
    , ActivityRange(..)
    , ActivityResponse
    , BreakdownItem
    , CashflowResponse
    , CategoryBreakdown
    , DirectionBreakdown
    , Kpi
    , NetWorthResponse
    , PerformancePoint
    , TimelinePoint
    )

import Data.AccountType exposing (AccountType)
import Data.Completeness exposing (Completeness)
import Data.Date exposing (Date)
import Data.Decimal exposing (Decimal)
import Data.Granularity exposing (Granularity)
import Data.Metadata exposing (Metadata)
import Data.Transaction exposing (Transaction)


type alias BreakdownItem =
    { account : String
    , accountType : Maybe AccountType
    , quantity : Decimal
    }


type ActivityKind
    = IncomeActivity
    | ExpenseActivity
    | AssetActivity
    | LiabilityActivity


type ActivityRange
    = MonthActivity
    | YearActivity


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


type alias CategoryBreakdown =
    { period : String
    , total : Decimal
    , items : List BreakdownItem
    , otherItems : List BreakdownItem
    }


type alias PerformancePoint =
    { period : String
    , startDate : Date
    , netInvestment : Decimal
    , netWorth : Decimal
    , gainLoss : Decimal
    }


type alias NetWorthResponse =
    { metadata : Metadata
    , displayCommodity : String
    , granularity : Granularity
    , netWorth : Kpi
    , assets : Kpi
    , liabilities : Kpi
    , timeline : List TimelinePoint
    , assetsBreakdown : CategoryBreakdown
    , liabilitiesBreakdown : CategoryBreakdown
    , performance : List PerformancePoint
    , incompletePeriods : Int
    , warnings : List String
    }


type alias CashflowResponse =
    { metadata : Metadata
    , displayCommodity : String
    , granularity : Granularity
    , totalCount : Int
    , filteredCount : Int
    , timelineAvailable : Bool
    , timeline : List TimelinePoint
    , incomeBreakdown : CategoryBreakdown
    , expenseBreakdown : CategoryBreakdown
    , warnings : List String
    }


type alias ActivityResponse =
    { metadata : Metadata
    , displayCommodity : String
    , kind : ActivityKind
    , range : ActivityRange
    , period : String
    , fiscalYearStartMonth : Int
    , buckets : List ActivityBucket
    , warnings : List String
    }
