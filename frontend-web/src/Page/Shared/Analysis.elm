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
import View.Analytics as AnalyticsView exposing (LegendState(..))



-- MODEL


type Model
    = Model ModelData


type alias ModelData =
    { granularity : Granularity
    , depth : Depth
    , period : Maybe Period
    , calendarMonth : Maybe Month
    , timeline : List Analytics.TimelinePoint
    , positive : SideState
    , negative : SideState
    , mode : Mode
    , fiscalYearStartMonth : FiscalYearStartMonth
    }


type alias SideState =
    { activity : ActivityState
    , otherExpanded : Bool
    , legendState : LegendState
    , selectedYearBucket : Maybe Analytics.ActivityBucket
    , donutHighlight : Maybe Int
    , calendarHighlight : Maybe Int
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
        , period = Nothing
        , calendarMonth = Nothing
        , timeline = []
        , positive = initialSide
        , negative = initialSide
        , mode = Exploring Nothing
        , fiscalYearStartMonth = FiscalYearStartMonth 4
        }


initialSide : SideState
initialSide =
    { activity = ActivityNotRequested
    , otherExpanded = False
    , legendState = LegendCollapsed
    , selectedYearBucket = Nothing
    , donutHighlight = Nothing
    , calendarHighlight = Nothing
    }



-- UPDATE


type Msg
    = GranularitySelected Granularity
    | TimelineSelected String
    | PeriodChanged Int
    | DepthSelected Int
    | OtherToggled Side
    | LegendToggled Side
    | LegendCategoryHighlighted Side Int
    | DonutSliceClicked Side Int
    | ActivityLoaded Side (Result ApiError Analytics.ActivityResponse)
    | BucketSelected Side Analytics.ActivityBucket
    | YearBucketSelected Side Analytics.ActivityBucket
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
                        , period = Nothing
                        , calendarMonth = Nothing
                        , positive = beginSideLoading data.positive
                        , negative = beginSideLoading data.negative
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
                            selectPeriod selectedPeriod selectedMonth data
                    in
                    withReportAndActivities context next

                _ ->
                    ( Model data, [] )

        PeriodChanged offset ->
            case ( data.period, data.calendarMonth ) of
                ( Just currentPeriod, Just currentMonth ) ->
                    let
                        monthDistance =
                            if data.granularity == Monthly then
                                offset

                            else
                                offset * 12

                        next =
                            selectPeriod
                                (shiftPeriod data.granularity offset currentPeriod)
                                (Just (Time.shiftMonth monthDistance currentMonth))
                                data
                    in
                    withReportAndActivities context next

                _ ->
                    ( Model data, [] )

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

        LegendToggled side ->
            let
                sideState =
                    sideFor side data

                nextLegendState =
                    case sideState.legendState of
                        LegendCollapsed ->
                            LegendExpanded

                        LegendExpanded ->
                            LegendCollapsed

                next =
                    setSide side { sideState | legendState = nextLegendState } data
            in
            ( Model next, [] )

        LegendCategoryHighlighted side index ->
            let
                sideState =
                    sideFor side data

                nextIndex =
                    if sideState.calendarHighlight == Just index then
                        Nothing

                    else
                        Just index
            in
            ( Model (setSide side { sideState | calendarHighlight = nextIndex } data), [] )

        DonutSliceClicked side index ->
            let
                sideState =
                    sideFor side data

                nextIndex =
                    if sideState.donutHighlight == Just index then
                        Nothing

                    else
                        Just index
            in
            ( Model (setSide side { sideState | donutHighlight = nextIndex } data), [] )

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

        YearBucketSelected side bucket ->
            let
                sideState =
                    sideFor side data

                nextBucket =
                    case sideState.selectedYearBucket of
                        Just selected ->
                            if selected.key == bucket.key then
                                Nothing

                            else
                                Just bucket

                        Nothing ->
                            Just bucket
            in
            ( Model (setSide side { sideState | selectedYearBucket = nextBucket } data), [] )

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
        next =
            { data
                | period = Nothing
                , calendarMonth = Nothing
                , positive = beginSideLoading data.positive
                , negative = beginSideLoading data.negative
                , mode = Exploring Nothing
            }
    in
    withReport context next


reportLoaded : Context -> Report -> Model -> ( Model, List Effect )
reportLoaded context report (Model data) =
    let
        points =
            timelinePoints report.timeline

        period =
            syncedPeriod data.period points data.granularity

        pointMonth =
            period
                |> Maybe.andThen (Time.periodToString >> pointFor points)
                |> Maybe.map (.startDate >> Time.monthFromDate)

        requestedMonth =
            if period == data.period then
                data.calendarMonth

            else
                Nothing

        calendarMonth =
            requestedMonth
                |> orElse pointMonth
                |> orElse (Just (Month { year = 1970, month = 1 }))

        next =
            { data
                | timeline = points
                , period = period
                , calendarMonth = calendarMonth
            }
    in
    ( Model next, activityEffects context next )


reportRequest : String -> Model -> Analytics.AnalyticsRequest
reportRequest query (Model data) =
    { query = query
    , granularity = data.granularity
    , displayCommodity = Nothing
    , positivePeriod = Maybe.map Time.periodToString data.period
    , negativePeriod = Maybe.map Time.periodToString data.period
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
    data.calendarMonth
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
        , compositionHeader theme data
        , breakdownsView theme device config report data
        , activityViews theme device config report data
        ]


compositionHeader : Theme -> ModelData -> Element Msg
compositionHeader theme data =
    Element.wrappedRow [ Element.width Element.fill, Element.spacing 12 ]
        [ Element.el [ Element.width Element.fill ]
            (Typography.sectionTitle theme "Composition and activity")
        , Element.column [ Element.width Element.fill, Element.spacing 8 ]
            [ Element.row [ Element.width Element.fill, Element.spacing 8 ]
                [ Element.el [ Element.width (Element.fillPortion 1), Element.alignLeft ]
                    (Typography.mutedBody theme "Period")
                , Element.row [ Element.width (Element.fillPortion 2) ]
                    [ Element.el [ Element.width Element.fill ] Element.none
                    , Ui.periodNavigation theme
                        { label =
                            Element.el [ Font.semiBold ]
                                (Element.text (selectedPeriodLabel data))
                        , onPrevious = PeriodChanged -1
                        , onNext = PeriodChanged 1
                        }
                    ]
                ]
            , Element.row [ Element.width Element.fill, Element.spacing 10 ]
                [ Element.el [ Element.width (Element.fillPortion 1), Element.alignLeft ]
                    (Typography.mutedBody theme "Account depth")
                , Element.row [ Element.width (Element.fillPortion 2) ]
                    [ Element.el [ Element.width Element.fill ] Element.none
                    , AnalyticsView.depthControl theme (Time.depthValue data.depth) DepthSelected
                    ]
                ]
            ]
        ]


selectedPeriodLabel : ModelData -> String
selectedPeriodLabel data =
    case data.granularity of
        Monthly ->
            data.period
                |> Maybe.map (Time.periodToString >> Calendar.periodLabel Monthly)
                |> Maybe.withDefault "Loading…"

        Yearly ->
            data.calendarMonth
                |> Maybe.map
                    (Time.monthToString
                        >> Calendar.fiscalPeriodForMonth
                            (Time.fiscalYearStartMonthValue data.fiscalYearStartMonth)
                        >> Calendar.fiscalPeriodLabel
                    )
                |> Maybe.withDefault "Loading…"


timelineView : Theme -> Int -> Config -> Report -> ModelData -> Element Msg
timelineView theme chartWidth config report data =
    case report.timeline of
        TimelineAvailable points ->
            Ui.card theme
                [ Typography.subsectionTitle theme config.timelineTitle
                , AnalyticsView.timelineChart theme
                    { commodity = report.displayCommodity
                    , points = points
                    , selected = Maybe.map Time.periodToString data.period
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
        , otherExpanded = sideState.otherExpanded
        , onOtherToggle = OtherToggled side
        , highlightedCategory = sideState.donutHighlight
        , onSliceClick = DonutSliceClicked side
        }


activityViews : Theme -> Device -> Config -> Report -> ModelData -> Element Msg
activityViews theme device config report data =
    responsiveGrid device
        [ activityView theme device data.granularity config.positive Positive report.displayCommodity data
        , activityView theme device data.granularity config.negative Negative report.displayCommodity data
        ]


activityView : Theme -> Device -> Granularity -> SideConfig -> Side -> String -> ModelData -> Element Msg
activityView theme device granularity config side commodity data =
    let
        sideState =
            sideFor side data

        month =
            data.calendarMonth
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
                , onSelect = BucketSelected side
                , onTransaction = TransactionSelected
                , device = device
                , legendState = sideState.legendState
                , onLegendToggle = LegendToggled side
                , highlightedCategory = sideState.calendarHighlight
                , onCategoryHighlight = LegendCategoryHighlighted side
                }

        Yearly ->
            AnalyticsView.activityYear theme
                { title = config.yearActivityTitle
                , commodity = commodity
                , tone = config.tone
                , response = activityValue sideState.activity
                , selectedBucket = sideState.selectedYearBucket
                , onSelect = YearBucketSelected side
                , device = device
                , legendState = sideState.legendState
                , onLegendToggle = LegendToggled side
                , highlightedCategory = sideState.calendarHighlight
                , onCategoryHighlight = LegendCategoryHighlighted side
                }


transaction : Model -> Maybe Transaction
transaction (Model data) =
    case data.mode of
        Exploring _ ->
            Nothing

        ViewingTransaction _ selectedTransaction ->
            Just selectedTransaction


beginSideLoading : SideState -> SideState
beginSideLoading sideState =
    { sideState
        | activity = beginActivityLoading sideState.activity
        , selectedYearBucket = Nothing
        , legendState = LegendCollapsed
        , donutHighlight = Nothing
        , calendarHighlight = Nothing
    }


selectPeriod : Period -> Maybe Month -> ModelData -> ModelData
selectPeriod period calendarMonth data =
    { data
        | period = Just period
        , calendarMonth = calendarMonth
        , positive = beginSideLoading data.positive
        , negative = beginSideLoading data.negative
        , mode = Exploring Nothing
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
