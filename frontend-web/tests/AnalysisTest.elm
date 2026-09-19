module AnalysisTest exposing (suite)

import Data.Analytics as Analytics exposing (TimelinePoint)
import Data.Completeness exposing (Completeness(..))
import Data.Date exposing (Date(..))
import Data.Decimal as Decimal
import Data.Freshness exposing (Freshness(..))
import Data.Granularity exposing (Granularity(..))
import Expect
import Page.Shared.Analysis as Analysis
    exposing
        ( Timeline(..)
        )
import Test exposing (Test, test)
import View.Analytics exposing (ActivityTone(..), Palette(..))


suite : Test
suite =
    Test.describe "calendar period selection"
        [ test "monthly navigation skips an empty month" <|
            \_ ->
                let
                    ( requested, requestEffects ) =
                        Analysis.requestReport context Analysis.init

                    loaded =
                        acceptReport requestEffects monthlyReport requested

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

                    effects =
                        reportEffects yearlyReport yearly
                in
                effects
                    |> List.filterMap activityPeriod
                    |> Expect.equal [ "2025-04", "2025-04" ]
        , test "a stale report cannot replace a newer request" staleReportTest
        , test "a stale activity cannot replace a newer side request" staleActivityTest
        ]


staleReportTest : () -> Expect.Expectation
staleReportTest _ =
    let
        ( first, firstEffects ) =
            Analysis.requestReport context Analysis.init

        ( second, _ ) =
            Analysis.requestReport context first
    in
    case Analysis.reportLoaded (reportToken firstEffects) context monthlyReport second of
        Nothing ->
            Expect.pass

        Just _ ->
            Expect.fail "the stale report was accepted"


staleActivityTest : () -> Expect.Expectation
staleActivityTest _ =
    let
        ( requested, requestEffects ) =
            Analysis.requestReport context Analysis.init

        ( loaded, oldEffects ) =
            Analysis.reportLoaded
                (reportToken requestEffects)
                context
                monthlyReport
                requested
                |> Maybe.withDefault ( requested, [] )

        ( newer, newerEffects ) =
            Analysis.update context (Analysis.PeriodChanged -1) loaded

        oldToken =
            activityToken Analysis.Positive oldEffects

        newToken =
            activityToken Analysis.Positive newerEffects

        ( accepted, _ ) =
            Analysis.update
                context
                (Analysis.ActivityLoaded Analysis.Positive newToken (Ok (activity "new")))
                newer

        ( afterStale, _ ) =
            Analysis.update
                context
                (Analysis.ActivityLoaded Analysis.Positive oldToken (Ok (activity "old")))
                accepted
    in
    Analysis.activityResponse Analysis.Positive afterStale
        |> Maybe.map .period
        |> Expect.equal (Just "new")


acceptReport : List Analysis.Effect -> Analysis.Report -> Analysis.Model -> Analysis.Model
acceptReport effects report model =
    Analysis.reportLoaded (reportToken effects) context report model
        |> Maybe.map Tuple.first
        |> Maybe.withDefault model


reportEffects : Analysis.Report -> Analysis.Model -> List Analysis.Effect
reportEffects report model =
    let
        ( requested, effects ) =
            Analysis.requestReport context model
    in
    Analysis.reportLoaded (reportToken effects) context report requested
        |> Maybe.map Tuple.second
        |> Maybe.withDefault []


reportToken : List Analysis.Effect -> Int
reportToken effects =
    effects
        |> List.filterMap
            (\effect ->
                case effect of
                    Analysis.FetchReport token _ ->
                        Just token

                    Analysis.FetchActivity _ _ _ ->
                        Nothing
            )
        |> List.head
        |> Maybe.withDefault -1


activityToken : Analysis.Side -> List Analysis.Effect -> Int
activityToken requestedSide effects =
    effects
        |> List.filterMap
            (\effect ->
                case effect of
                    Analysis.FetchActivity side token _ ->
                        if side == requestedSide then
                            Just token

                        else
                            Nothing

                    Analysis.FetchReport _ _ ->
                        Nothing
            )
        |> List.head
        |> Maybe.withDefault -1


activity : String -> Analytics.ActivityResponse
activity period =
    { metadata = { revision = Nothing, freshness = Fresh, warnings = [] }
    , displayCommodity = "INR"
    , kind = Analytics.IncomeActivity
    , range = Analytics.MonthActivity
    , period = period
    , fiscalYearStartMonth = 4
    , buckets = []
    , warnings = []
    }


activityPeriod : Analysis.Effect -> Maybe String
activityPeriod effect =
    case effect of
        Analysis.FetchActivity _ _ request ->
            Just request.period

        Analysis.FetchReport _ _ ->
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


monthlyReport : Analysis.Report
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


yearlyReport : Analysis.Report
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
