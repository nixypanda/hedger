module AnalysisTest exposing (suite)

import Api.Analytics as Analytics exposing (Timeline(..))
import Data.Analytics exposing (TimelinePoint)
import Data.Completeness exposing (Completeness(..))
import Data.Date exposing (Date(..))
import Data.Decimal as Decimal
import Data.Granularity exposing (Granularity(..))
import Expect
import Page.Shared.Analysis as Analysis
import Test exposing (Test, test)
import View.Analytics exposing (ActivityTone(..), Palette(..))


suite : Test
suite =
    Test.describe "calendar period selection"
        [ test "monthly navigation skips an empty month" <|
            \_ ->
                let
                    ( loaded, _ ) =
                        Analysis.reportLoaded context monthlyReport Analysis.init

                    ( _, effects ) =
                        Analysis.update context (Analysis.PeriodChanged -1) loaded
                in
                effects
                    |> List.filterMap activityPeriod
                    |> Expect.equal [ "2024-04", "2024-04" ]
        , test "yearly view requests the fiscal year beginning in the selected year" <|
            \_ ->
                let
                    ( yearly, _ ) =
                        Analysis.update context (Analysis.GranularitySelected Yearly) Analysis.init

                    ( _, effects ) =
                        Analysis.reportLoaded context yearlyReport yearly
                in
                effects
                    |> List.filterMap activityPeriod
                    |> Expect.equal [ "2025-04", "2025-04" ]
        ]


activityPeriod : Analysis.Effect -> Maybe String
activityPeriod effect =
    case effect of
        Analysis.FetchActivity _ request ->
            Just request.period

        Analysis.FetchReport _ ->
            Nothing


context : Analysis.Context
context =
    { query = ""
    , config =
        { timelineSectionTitle = "Timeline"
        , timelineTitle = "Activity"
        , netLabel = "Net"
        , positive = sideConfig Analytics.IncomeActivity PositivePalette IncomeTone
        , negative = sideConfig Analytics.ExpenseActivity NegativePalette ExpenseTone
        }
    }


sideConfig : Analytics.ActivityKind -> Palette -> ActivityTone -> Analysis.SideConfig
sideConfig kind palette tone =
    { label = "Side"
    , monthActivityTitle = "Month"
    , yearActivityTitle = "Year"
    , activityKind = kind
    , palette = palette
    , tone = tone
    }


monthlyReport : Analytics.Report
monthlyReport =
    { displayCommodity = "INR"
    , timeline =
        TimelineAvailable
            [ point "2024-04" 4
            , point "2024-06" 6
            ]
    , positiveBreakdown = breakdown
    , negativeBreakdown = breakdown
    , warnings = []
    }


yearlyReport : Analytics.Report
yearlyReport =
    { displayCommodity = "INR"
    , timeline = TimelineAvailable [ point "2025" 1 ]
    , positiveBreakdown = breakdown
    , negativeBreakdown = breakdown
    , warnings = []
    }


point : String -> Int -> TimelinePoint
point period month =
    { period = period
    , startDate = Date { year = 2024, month = month, day = 1 }
    , positive = Decimal.zero
    , negative = Decimal.zero
    , net = Decimal.zero
    , completeness = Complete
    , missingCommodities = []
    }


breakdown : Analytics.CategoryBreakdown
breakdown =
    { period = ""
    , total = Decimal.zero
    , items = []
    , otherItems = []
    }
