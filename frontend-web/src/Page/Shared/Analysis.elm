module Page.Shared.Analysis exposing
    ( Config
    , Context
    , Effect(..)
    , Model
    , Msg(..)
    , Side(..)
    , SideConfig
    , controlsView
    , init
    , queryChanged
    , reportLoaded
    , reportRequest
    , transaction
    , update
    , view
    )

import Api.Analytics as Analytics exposing (ActivityRange(..), Granularity(..), Report, Timeline(..))
import Api.Error exposing (ApiError)
import Api.Transaction exposing (Transaction)
import Data.Calendar as Calendar
import Data.Time as Time exposing (Depth(..), FiscalYearStartMonth(..), Month(..), Period(..))
import Element exposing (Device, Element)
import Element.Font as Font
import State.Viewport exposing (Viewport)
import Ui
import Ui.Responsive as Responsive
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import View.Analytics as AnalyticsView



-- MODEL


type Model
    = Model ModelData


type alias ModelData =
    { granularity : Granularity
    , depth : Depth
    , selectedTimeline : Maybe Period
    , timeline : List Analytics.TimelinePoint
    , positive : SideState
    , negative : SideState
    , mode : Mode
    , fiscalYearStartMonth : FiscalYearStartMonth
    }


type alias SideState =
    { period : Maybe Period
    , calendarMonth : Maybe Month
    , activity : ActivityState
    , otherExpanded : Bool
    }


type ActivityState
    = ActivityNotRequested
    | ActivityLoading (Maybe Analytics.ActivityResponse)
    | ActivityReady Analytics.ActivityResponse
    | ActivityFailed ApiError (Maybe Analytics.ActivityResponse)


type Mode
    = Exploring (Maybe Selection)
    | ViewingTransaction Selection Transaction


type Selection
    = Selection Side Analytics.ActivityBucket


type Side
    = Positive
    | Negative


type alias Context =
    { config : Config
    , query : String
    }


type alias Config =
    { timelineSectionTitle : String
    , timelineTitle : String
    , netLabel : String
    , positive : SideConfig
    , negative : SideConfig
    }


type alias SideConfig =
    { label : String
    , monthActivityTitle : String
    , yearActivityTitle : String
    , activityKind : Analytics.ActivityKind
    , palette : AnalyticsView.Palette
    , tone : AnalyticsView.ActivityTone
    }


init : Model
init =
    Model
        { granularity = Monthly
        , depth = Depth 2
        , selectedTimeline = Nothing
        , timeline = []
        , positive = initialSide
        , negative = initialSide
        , mode = Exploring Nothing
        , fiscalYearStartMonth = FiscalYearStartMonth 4
        }


initialSide : SideState
initialSide =
    { period = Nothing
    , calendarMonth = Nothing
    , activity = ActivityNotRequested
    , otherExpanded = False
    }



-- UPDATE


type Msg
    = GranularitySelected Granularity
    | TimelineSelected String
    | BreakdownPeriodChanged Side Int
    | DepthSelected Int
    | OtherToggled Side
    | CalendarPeriodChanged Side Int
    | ActivityLoaded Side (Result ApiError Analytics.ActivityResponse)
    | BucketSelected Side Analytics.ActivityBucket
    | TransactionSelected Transaction
    | Back


type Effect
    = FetchReport Analytics.AnalyticsRequest
    | FetchActivity Side Analytics.ActivityRequest


update : Context -> Msg -> Model -> ( Model, List Effect )
update context msg (Model data) =
    case msg of
        GranularitySelected granularity ->
            let
                next =
                    { data
                        | granularity = granularity
                        , selectedTimeline = Nothing
                        , positive = resetSideForGranularity data.positive
                        , negative = resetSideForGranularity data.negative
                        , mode = Exploring Nothing
                    }
            in
            withReportAndActivities context next

        TimelineSelected period ->
            case ( periodFromString data.granularity period, pointFor data.timeline period ) of
                ( Just selectedPeriod, Just point ) ->
                    let
                        selectedMonth =
                            Just (Time.monthFromDate point.startDate)

                        next =
                            { data
                                | selectedTimeline = Just selectedPeriod
                                , positive = selectPeriod selectedPeriod selectedMonth data.positive
                                , negative = selectPeriod selectedPeriod selectedMonth data.negative
                                , mode = Exploring Nothing
                            }
                    in
                    withReport context next

                _ ->
                    ( Model data, [] )

        BreakdownPeriodChanged side offset ->
            let
                sideState =
                    sideFor side data

                nextSide =
                    { sideState
                        | period = Maybe.map (shiftPeriod data.granularity offset) sideState.period
                    }
            in
            withReport context (setSide side nextSide data)

        DepthSelected requestedDepth ->
            case Time.depthFromInt requestedDepth of
                Nothing ->
                    ( Model data, [] )

                Just depth ->
                    withReportAndActivities context { data | depth = depth }

        OtherToggled side ->
            let
                sideState =
                    sideFor side data

                next =
                    setSide side { sideState | otherExpanded = not sideState.otherExpanded } data
            in
            ( Model next, [] )

        CalendarPeriodChanged side offset ->
            case (sideFor side data).calendarMonth of
                Nothing ->
                    ( Model data, [] )

                Just currentMonth ->
                    let
                        distance =
                            if activityRange data.granularity == MonthActivity then
                                offset

                            else
                                offset * 12

                        sideState =
                            sideFor side data

                        nextSide =
                            { sideState
                                | calendarMonth = Just (Time.shiftMonth distance currentMonth)
                                , activity = beginActivityLoading sideState.activity
                            }

                        next =
                            setSide side nextSide { data | mode = Exploring Nothing }
                    in
                    ( Model next, activityEffect context side next |> maybeToList )

        ActivityLoaded side result ->
            let
                previous =
                    (sideFor side data).activity

                remote =
                    case result of
                        Ok response ->
                            ActivityReady response

                        Err error ->
                            ActivityFailed error (activityValue previous)

                sideState =
                    sideFor side data

                fiscalYearStartMonth =
                    result
                        |> Result.toMaybe
                        |> Maybe.andThen (Time.fiscalYearStartMonthFromInt << .fiscalYearStartMonth)
                        |> Maybe.withDefault data.fiscalYearStartMonth
            in
            ( Model
                (setSide side
                    { sideState | activity = remote }
                    { data | fiscalYearStartMonth = fiscalYearStartMonth }
                )
            , []
            )

        BucketSelected side bucket ->
            let
                selection =
                    selectionFor data.mode

                nextSelection =
                    case selection of
                        Just (Selection selectedSide selectedBucket) ->
                            if selectedSide == side && selectedBucket.key == bucket.key then
                                Nothing

                            else
                                Just (Selection side bucket)

                        Nothing ->
                            Just (Selection side bucket)
            in
            ( Model { data | mode = Exploring nextSelection }, [] )

        TransactionSelected selectedTransaction ->
            case selectionFor data.mode of
                Nothing ->
                    ( Model data, [] )

                Just selection ->
                    ( Model { data | mode = ViewingTransaction selection selectedTransaction }, [] )

        Back ->
            ( Model { data | mode = Exploring (selectionFor data.mode) }, [] )


queryChanged : Context -> Model -> ( Model, List Effect )
queryChanged context (Model data) =
    let
        resetSide sideState =
            { sideState | period = Nothing }

        next =
            { data
                | selectedTimeline = Nothing
                , positive = resetSide data.positive
                , negative = resetSide data.negative
                , mode = Exploring Nothing
            }
    in
    withReport context next


reportLoaded : Context -> Report -> Model -> ( Model, List Effect )
reportLoaded context report (Model data) =
    let
        points =
            timelinePoints report.timeline

        selectedTimeline =
            syncedPeriod data.selectedTimeline points data.granularity

        selectedMonth =
            selectedTimeline
                |> Maybe.andThen (Time.periodToString >> pointFor points)
                |> orElse (List.reverse points |> List.head)
                |> Maybe.map (.startDate >> Time.monthFromDate)
                |> orElse (Just (Month { year = 1970, month = 1 }))

        updateSide breakdown sideState =
            { sideState
                | period = periodFromString data.granularity breakdown.period
                , calendarMonth = sideState.calendarMonth |> orElse selectedMonth
            }

        next =
            { data
                | timeline = points
                , selectedTimeline = selectedTimeline
                , positive = updateSide report.positiveBreakdown data.positive
                , negative = updateSide report.negativeBreakdown data.negative
            }
    in
    ( Model next, activityEffects context next )


reportRequest : String -> Model -> Analytics.AnalyticsRequest
reportRequest query (Model data) =
    { query = query
    , granularity = data.granularity
    , displayCommodity = Nothing
    , positivePeriod = Maybe.map Time.periodToString data.positive.period
    , negativePeriod = Maybe.map Time.periodToString data.negative.period
    , positiveDepth = Time.depthValue data.depth
    , negativeDepth = Time.depthValue data.depth
    }


withReport : Context -> ModelData -> ( Model, List Effect )
withReport context data =
    ( Model data, [ FetchReport (reportRequest context.query (Model data)) ] )


withReportAndActivities : Context -> ModelData -> ( Model, List Effect )
withReportAndActivities context data =
    ( Model data
    , FetchReport (reportRequest context.query (Model data)) :: activityEffects context data
    )


activityEffects : Context -> ModelData -> List Effect
activityEffects context data =
    List.filterMap (\side -> activityEffect context side data) [ Positive, Negative ]


activityEffect : Context -> Side -> ModelData -> Maybe Effect
activityEffect context side data =
    let
        sideState =
            sideFor side data
    in
    sideState.calendarMonth
        |> Maybe.map
            (\month ->
                FetchActivity side
                    { kind = (sideConfigFor side context.config).activityKind
                    , range = activityRange data.granularity
                    , period =
                        if activityRange data.granularity == MonthActivity then
                            Time.monthToString month

                        else
                            Calendar.fiscalPeriodForMonth
                                (Time.fiscalYearStartMonthValue data.fiscalYearStartMonth)
                                (Time.monthToString month)
                    , query = context.query
                    , displayCommodity = Nothing
                    , depth = Time.depthValue data.depth
                    }
            )


controlsView : Theme -> Model -> Element Msg
controlsView theme (Model data) =
    Ui.wideSegmentedControl theme
        [ { label = "M", selected = data.granularity == Monthly, onPress = GranularitySelected Monthly }
        , { label = "Y", selected = data.granularity == Yearly, onPress = GranularitySelected Yearly }
        ]



-- VIEW


view : Theme -> Viewport -> Config -> Report -> Model -> Element Msg
view theme viewport config report (Model data) =
    let
        device =
            Element.classifyDevice viewport

        chartWidth =
            max 320 (Responsive.usableContentWidth viewport device - 40)
    in
    Element.column [ Element.width Element.fill, Element.spacing 16 ]
        [ Typography.sectionTitle theme config.timelineSectionTitle
        , timelineView theme chartWidth config report data
        , AnalyticsView.warningList theme report.warnings
        , compositionHeader theme data.depth
        , breakdownsView theme device config report data
        , activityViews theme device config report data
        ]


compositionHeader : Theme -> Depth -> Element Msg
compositionHeader theme depth =
    Element.wrappedRow [ Element.width Element.fill, Element.spacing 12 ]
        [ Element.el [ Element.width Element.fill ]
            (Typography.sectionTitle theme "Composition and activity")
        , Element.row [ Element.spacing 10 ]
            [ Typography.mutedBody theme "Account depth"
            , AnalyticsView.depthControl theme (Time.depthValue depth) DepthSelected
            ]
        ]


timelineView : Theme -> Int -> Config -> Report -> ModelData -> Element Msg
timelineView theme chartWidth config report data =
    case report.timeline of
        TimelineAvailable points ->
            Ui.card theme
                [ Typography.subsectionTitle theme config.timelineTitle
                , AnalyticsView.timelineChart theme
                    { commodity = report.displayCommodity
                    , points = points
                    , selected = Maybe.map Time.periodToString data.selectedTimeline
                    , onSelect = TimelineSelected
                    , availableWidth = chartWidth
                    , positiveLabel = config.positive.label
                    , negativeLabel = config.negative.label
                    , netLabel = config.netLabel
                    }
                , chartLegend theme
                    [ ( theme.positive, config.positive.label )
                    , ( theme.negative, config.negative.label )
                    , ( theme.primary, config.netLabel )
                    ]
                ]

        TimelineUnavailable message _ ->
            Ui.errorBanner theme message


breakdownsView : Theme -> Device -> Config -> Report -> ModelData -> Element Msg
breakdownsView theme device config report data =
    responsiveGrid device
        [ categoryView
            theme
            config.positive
            Positive
            report.displayCommodity
            report.positiveBreakdown
            data.positive
        , categoryView
            theme
            config.negative
            Negative
            report.displayCommodity
            report.negativeBreakdown
            data.negative
        ]


categoryView :
    Theme
    -> SideConfig
    -> Side
    -> String
    -> Analytics.CategoryBreakdown
    -> SideState
    -> Element Msg
categoryView theme config side commodity breakdown sideState =
    AnalyticsView.categoryCard theme
        { title = config.label
        , commodity = commodity
        , breakdown = breakdown
        , palette = config.palette
        , onPrevious = BreakdownPeriodChanged side -1
        , onNext = BreakdownPeriodChanged side 1
        , otherExpanded = sideState.otherExpanded
        , onOtherToggle = OtherToggled side
        }


activityViews : Theme -> Device -> Config -> Report -> ModelData -> Element Msg
activityViews theme device config report data =
    responsiveGrid device
        [ activityView theme data.granularity config.positive Positive report.displayCommodity data
        , activityView theme data.granularity config.negative Negative report.displayCommodity data
        ]


activityView : Theme -> Granularity -> SideConfig -> Side -> String -> ModelData -> Element Msg
activityView theme granularity config side commodity data =
    let
        sideState =
            sideFor side data

        month =
            sideState.calendarMonth
                |> Maybe.map Time.monthToString
                |> Maybe.withDefault "1970-01"
    in
    case granularity of
        Monthly ->
            AnalyticsView.activityMonth theme
                { title = config.monthActivityTitle
                , period = month
                , commodity = commodity
                , tone = config.tone
                , response = activityValue sideState.activity
                , selectedBucket = selectedBucketFor side data.mode
                , onPrevious = CalendarPeriodChanged side -1
                , onNext = CalendarPeriodChanged side 1
                , onSelect = BucketSelected side
                , onTransaction = TransactionSelected
                }

        Yearly ->
            AnalyticsView.activityYear theme
                { title = config.yearActivityTitle
                , period =
                    Calendar.fiscalPeriodForMonth
                        (Time.fiscalYearStartMonthValue data.fiscalYearStartMonth)
                        month
                , commodity = commodity
                , tone = config.tone
                , response = activityValue sideState.activity
                , onPrevious = CalendarPeriodChanged side -1
                , onNext = CalendarPeriodChanged side 1
                }


transaction : Model -> Maybe Transaction
transaction (Model data) =
    case data.mode of
        Exploring _ ->
            Nothing

        ViewingTransaction _ selectedTransaction ->
            Just selectedTransaction


resetSideForGranularity : SideState -> SideState
resetSideForGranularity sideState =
    { sideState
        | period = Nothing
        , activity = beginActivityLoading sideState.activity
    }


selectPeriod : Period -> Maybe Month -> SideState -> SideState
selectPeriod period selectedMonth sideState =
    { sideState
        | period = Just period
        , calendarMonth = selectedMonth |> orElse sideState.calendarMonth
    }


activityRange : Granularity -> ActivityRange
activityRange granularity =
    case granularity of
        Monthly ->
            MonthActivity

        Yearly ->
            YearActivity


sideFor : Side -> ModelData -> SideState
sideFor side data =
    case side of
        Positive ->
            data.positive

        Negative ->
            data.negative


setSide : Side -> SideState -> ModelData -> ModelData
setSide side sideState data =
    case side of
        Positive ->
            { data | positive = sideState }

        Negative ->
            { data | negative = sideState }


sideConfigFor : Side -> Config -> SideConfig
sideConfigFor side config =
    case side of
        Positive ->
            config.positive

        Negative ->
            config.negative


selectionFor : Mode -> Maybe Selection
selectionFor mode =
    case mode of
        Exploring selection ->
            selection

        ViewingTransaction selection _ ->
            Just selection


selectedBucketFor : Side -> Mode -> Maybe Analytics.ActivityBucket
selectedBucketFor side mode =
    case selectionFor mode of
        Just (Selection selectedSide bucket) ->
            if selectedSide == side then
                Just bucket

            else
                Nothing

        Nothing ->
            Nothing


activityValue : ActivityState -> Maybe Analytics.ActivityResponse
activityValue activity =
    case activity of
        ActivityNotRequested ->
            Nothing

        ActivityLoading stale ->
            stale

        ActivityReady response ->
            Just response

        ActivityFailed _ stale ->
            stale


beginActivityLoading : ActivityState -> ActivityState
beginActivityLoading activity =
    ActivityLoading (activityValue activity)


timelinePoints : Timeline -> List Analytics.TimelinePoint
timelinePoints timeline =
    case timeline of
        TimelineAvailable points ->
            points

        TimelineUnavailable _ points ->
            points


orElse : Maybe value -> Maybe value -> Maybe value
orElse fallback maybeValue =
    case maybeValue of
        Just _ ->
            maybeValue

        Nothing ->
            fallback


maybeToList : Maybe value -> List value
maybeToList maybeValue =
    case maybeValue of
        Just value ->
            [ value ]

        Nothing ->
            []


syncedPeriod : Maybe Period -> List { point | period : String } -> Granularity -> Maybe Period
syncedPeriod requested points granularity =
    case requested of
        Just period ->
            if List.any (periodMatches granularity period) points then
                Just period

            else
                latestPeriod granularity points

        Nothing ->
            latestPeriod granularity points


latestPeriod : Granularity -> List { point | period : String } -> Maybe Period
latestPeriod granularity points =
    List.reverse points
        |> List.filterMap (.period >> periodFromString granularity)
        |> List.head


periodMatches : Granularity -> Period -> { point | period : String } -> Bool
periodMatches granularity period point =
    periodFromString granularity point.period == Just period


shiftPeriod : Granularity -> Int -> Period -> Period
shiftPeriod granularity offset period =
    case ( granularity, period ) of
        ( Monthly, MonthPeriod month ) ->
            MonthPeriod (Time.shiftMonth offset month)

        ( Yearly, YearPeriod year ) ->
            YearPeriod (year + offset)

        ( Monthly, YearPeriod year ) ->
            YearPeriod year

        ( Yearly, MonthPeriod month ) ->
            MonthPeriod month


pointFor : List Analytics.TimelinePoint -> String -> Maybe Analytics.TimelinePoint
pointFor points period =
    List.filter (.period >> (==) period) points |> List.head


periodFromString : Granularity -> String -> Maybe Period
periodFromString granularity value =
    case granularity of
        Monthly ->
            Time.monthFromString value |> Maybe.map MonthPeriod

        Yearly ->
            if String.length value == 4 && String.all Char.isDigit value then
                String.toInt value |> Maybe.map YearPeriod

            else
                Nothing


responsiveGrid : Device -> List (Element msg) -> Element msg
responsiveGrid device items =
    if Ui.isSmall device then
        Element.column [ Element.width Element.fill, Element.spacing 16 ] items

    else
        Element.row [ Element.width Element.fill, Element.spacing 16 ] items


chartLegend : Theme -> List ( Element.Color, String ) -> Element msg
chartLegend theme items =
    Element.wrappedRow [ Element.spacing 14 ]
        (List.map
            (\( color, label ) ->
                Element.row [ Element.spacing 5 ]
                    [ Ui.accentDot color
                    , Element.el [ Font.color theme.textMuted ] (Element.text label)
                    ]
            )
            items
        )
