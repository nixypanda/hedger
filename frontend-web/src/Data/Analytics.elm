module Data.Analytics exposing
    ( ActivityBucket
    , ActivityDirection(..)
    , ActivityKind(..)
    , ActivityMovement(..)
    , ActivityRange(..)
    , ActivityResponse
    , AssetEvent(..)
    , AssetFigures
    , BreakdownItem
    , CashflowResponse
    , CategoryBreakdown
    , DirectionBreakdown
    , FlowEvent(..)
    , FlowFigures
    , Kpi
    , LiabilityEvent(..)
    , LiabilityFigures
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
    , categories : List BreakdownItem
    , movement : ActivityMovement
    , transactions : List Transaction
    }


type ActivityMovement
    = AssetMovements AssetFigures
    | LiabilityMovements LiabilityFigures
    | IncomeMovements FlowFigures
    | ExpenseMovements FlowFigures


type alias AssetFigures =
    { acquired : Decimal
    , disposed : Decimal
    , transferred : Decimal
    , directions : List DirectionBreakdown
    , events : List AssetEvent
    }


type AssetEvent
    = Acquired String Decimal
    | Disposed String Decimal
    | Transferred String Decimal String Decimal


type alias LiabilityFigures =
    { borrowed : Decimal
    , repaid : Decimal
    , forgiven : Decimal
    , borrowingReversed : Decimal
    , netDebtMovement : Decimal
    , directions : List DirectionBreakdown
    , events : List LiabilityEvent
    }


type LiabilityEvent
    = Borrowed String Decimal
    | Repaid String Decimal
    | Refinanced String Decimal String Decimal
    | Forgiven String Decimal
    | BorrowingReversed String Decimal


type alias FlowFigures =
    { posted : Decimal
    , reversed : Decimal
    , events : List FlowEvent
    }


type FlowEvent
    = FlowPosted String Decimal
    | FlowReversed String Decimal


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
