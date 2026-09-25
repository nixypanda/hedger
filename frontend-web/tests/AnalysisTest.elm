module AnalysisTest exposing (suite)

import Data.Analytics as Analytics exposing (ActivityResponse, TimelinePoint)
import Data.Completeness exposing (Completeness(..))
import Data.Date exposing (Date(..))
import Data.Decimal as Decimal
import Data.Freshness exposing (Freshness(..))
import Data.Granularity exposing (Granularity(..))
import Expect
import Fuzz
import Page.Shared.Analysis as Analysis exposing (Timeline(..))
import Test exposing (Test, describe, fuzz)
import View.Analytics exposing (ActivityTone(..), Palette(..))


{-| Two domain rules of the analysis views: an out-of-date report or activity
response can never overwrite a newer one, and period selection follows the
fiscal calendar (a yearly view requests the fiscal year beginning in the
selected year; monthly navigation only moves between reported periods).
-}
suite : Test
suite =
    describe "calendar period selection"
        [ fuzz reportFuzzer "stale responses cannot overwrite newer requests" staleResponseHolds
        , fuzz periodFuzzer "period selection follows the fiscal calendar" periodSelectionHolds
        ]



-- STALENESS


staleResponseHolds : Analysis.Report -> Expect.Expectation
staleResponseHolds report =
    Expect.all
        [ \_ -> reportStalenessHolds report
        , \_ -> activityStalenessHolds report
        ]
        ()


reportStalenessHolds : Analysis.Report -> Expect.Expectation
reportStalenessHolds report =
    let
        ( first, firstEffects ) =
            Analysis.requestReport context Analysis.init

        firstToken =
            reportToken firstEffects

        ( second, secondEffects ) =
            Analysis.requestReport context first

        secondToken =
            reportToken secondEffects
    in
    case Analysis.reportLoaded firstToken context report second of
        Just _ ->
            Expect.fail "A report loaded for an older request must be rejected."

        Nothing ->
            case Analysis.reportLoaded secondToken context report second of
                Just _ ->
                    Expect.pass

                Nothing ->
                    Expect.fail "The newest report request must load."


activityStalenessHolds : Analysis.Report -> Expect.Expectation
activityStalenessHolds report =
    case loadReport report Analysis.init of
        Nothing ->
            Expect.fail "The first report must load."

        Just ( loaded, olderEffects ) ->
            case loadReport report loaded of
                Nothing ->
                    Expect.fail "The second report must load."

                Just ( newer, newerEffects ) ->
                    compareActivityTokens
                        (activityToken Analysis.Positive olderEffects)
                        (activityToken Analysis.Positive newerEffects)
                        newer


compareActivityTokens : Maybe Int -> Maybe Int -> Analysis.Model -> Expect.Expectation
compareActivityTokens olderRequest newerRequest newer =
    case ( olderRequest, newerRequest ) of
        ( Just olderToken, Just newerToken ) ->
            let
                accepted =
                    applyActivity Analysis.Positive newerToken (activity "new") newer

                afterStale =
                    applyActivity Analysis.Positive olderToken (activity "old") accepted
            in
            Expect.all
                [ \_ -> expectActivityPeriod "new" accepted
                , \_ -> expectActivityPeriod "new" afterStale
                ]
                ()

        _ ->
            Expect.fail "Expected an activity request for each report load."


expectActivityPeriod : String -> Analysis.Model -> Expect.Expectation
expectActivityPeriod expected model =
    Analysis.activityResponse Analysis.Positive model
        |> Maybe.map .period
        |> Expect.equal (Just expected)



-- FISCAL PERIOD SELECTION


periodSelectionHolds : PeriodInput -> Expect.Expectation
periodSelectionHolds input =
    Expect.all
        [ \_ -> monthlyNavigationHolds input.periods
        , \_ -> yearlyFiscalHolds input.year input.fiscalStart input.periods
        ]
        ()


monthlyNavigationHolds : List String -> Expect.Expectation
monthlyNavigationHolds periods =
    case List.reverse periods of
        _ :: previous :: _ ->
            case loadReport (monthlyReportFor periods) Analysis.init of
                Nothing ->
                    Expect.fail "The monthly report must load."

                Just ( loaded, _ ) ->
                    let
                        navigated =
                            Analysis.update context (Analysis.PeriodChanged -1) loaded

                        requested =
                            Tuple.second navigated
                                |> List.filterMap activityPeriod

                        pastLatest =
                            Analysis.update context (Analysis.PeriodChanged 1) loaded
                                |> Tuple.second
                                |> List.filterMap activityPeriod
                    in
                    Expect.all
                        [ \_ -> Expect.equal [ previous, previous ] requested
                        , \_ -> Expect.equal [] pastLatest
                        ]
                        ()

        _ ->
            Expect.fail "The monthly timeline needs at least two periods."


yearlyFiscalHolds : Int -> Int -> List String -> Expect.Expectation
yearlyFiscalHolds year fiscalStart periods =
    case loadReport (monthlyReportFor periods) Analysis.init of
        Nothing ->
            Expect.fail "The initial report must load."

        Just ( loaded, effects ) ->
            case activityToken Analysis.Positive effects of
                Nothing ->
                    Expect.fail "The initial report must request activity."

                Just token ->
                    let
                        fiscalActivity =
                            activityWithFiscal fiscalStart

                        withFiscal =
                            applyActivity Analysis.Positive token fiscalActivity loaded

                        yearly =
                            applyGranularity Yearly withFiscal

                        ( requested, requestedEffects ) =
                            Analysis.requestReport context yearly

                        loadedYearly =
                            Analysis.reportLoaded
                                (reportToken requestedEffects)
                                context
                                (yearlyReportFor year)
                                requested
                    in
                    case loadedYearly of
                        Nothing ->
                            Expect.fail "The yearly report must load."

                        Just ( _, yearlyEffects ) ->
                            let
                                expected =
                                    monthKey ( year, fiscalStart )
                            in
                            yearlyEffects
                                |> List.filterMap activityPeriod
                                |> Expect.equal [ expected, expected ]



-- HELPERS


loadReport : Analysis.Report -> Analysis.Model -> Maybe ( Analysis.Model, List Analysis.Effect )
loadReport report model =
    let
        ( requested, effects ) =
            Analysis.requestReport context model
    in
    Analysis.reportLoaded (reportToken effects) context report requested


applyActivity : Analysis.Side -> Int -> ActivityResponse -> Analysis.Model -> Analysis.Model
applyActivity side token response model =
    Analysis.update
        context
        (Analysis.ActivityLoaded side token (Ok response))
        model
        |> Tuple.first


applyGranularity : Granularity -> Analysis.Model -> Analysis.Model
applyGranularity granularity model =
    Analysis.update context (Analysis.GranularitySelected granularity) model
        |> Tuple.first


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


activityToken : Analysis.Side -> List Analysis.Effect -> Maybe Int
activityToken side effects =
    effects
        |> List.filterMap
            (\effect ->
                case effect of
                    Analysis.FetchActivity requestedSide token _ ->
                        if requestedSide == side then
                            Just token

                        else
                            Nothing

                    Analysis.FetchReport _ _ ->
                        Nothing
            )
        |> List.head


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


activity : String -> ActivityResponse
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


activityWithFiscal : Int -> ActivityResponse
activityWithFiscal fiscalStart =
    let
        base =
            activity "ignored"
    in
    { base | fiscalYearStartMonth = fiscalStart }


monthlyReportFor : List String -> Analysis.Report
monthlyReportFor periods =
    { displayCommodity = "INR"
    , timeline = TimelineAvailable (List.map monthPoint periods)
    , positiveBreakdown = breakdown
    , negativeBreakdown = breakdown
    , warnings = []
    }


yearlyReportFor : Int -> Analysis.Report
yearlyReportFor year =
    { displayCommodity = "INR"
    , timeline = TimelineAvailable [ pointWith (String.fromInt year) ( year, 1 ) ]
    , positiveBreakdown = breakdown
    , negativeBreakdown = breakdown
    , warnings = []
    }


monthPoint : String -> TimelinePoint
monthPoint period =
    case String.split "-" period of
        [ yearText, monthText ] ->
            pointWith period
                ( Maybe.withDefault 2020 (String.toInt yearText)
                , Maybe.withDefault 1 (String.toInt monthText)
                )

        _ ->
            pointWith period ( 2020, 1 )


pointWith : String -> ( Int, Int ) -> TimelinePoint
pointWith period ( year, month ) =
    { period = period
    , startDate = Date { year = year, month = month, day = 1 }
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


monthKey : ( Int, Int ) -> String
monthKey ( year, month ) =
    String.fromInt year ++ "-" ++ String.padLeft 2 '0' (String.fromInt month)



-- FUZZERS


type alias PeriodInput =
    { year : Int
    , fiscalStart : Int
    , periods : List String
    }


periodFuzzer : Fuzz.Fuzzer PeriodInput
periodFuzzer =
    Fuzz.map3 PeriodInput
        (Fuzz.intRange 2018 2028)
        (Fuzz.intRange 1 12)
        periodsFuzzer


reportFuzzer : Fuzz.Fuzzer Analysis.Report
reportFuzzer =
    Fuzz.map monthlyReportFor periodsFuzzer


periodsFuzzer : Fuzz.Fuzzer (List String)
periodsFuzzer =
    Fuzz.map3
        (\year month offsets ->
            (0 :: 1 :: offsets)
                |> unique
                |> List.sort
                |> List.map (\offset -> monthKey (addMonths offset ( year, month )))
        )
        (Fuzz.intRange 2020 2026)
        (Fuzz.intRange 1 12)
        (Fuzz.list (Fuzz.intRange 0 12))


unique : List Int -> List Int
unique values =
    List.foldl
        (\value seen ->
            if List.member value seen then
                seen

            else
                seen ++ [ value ]
        )
        []
        values


addMonths : Int -> ( Int, Int ) -> ( Int, Int )
addMonths offset ( year, month ) =
    let
        zeroBased =
            year * 12 + (month - 1) + offset
    in
    ( zeroBased // 12, modBy 12 zeroBased + 1 )
