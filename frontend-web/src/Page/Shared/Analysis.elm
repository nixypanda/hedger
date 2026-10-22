module Page.Shared.Analysis exposing
    ( Config
    , Context
    , Effect(..)
    , Model
    , Msg(..)
    , Report
    , Side(..)
    , SideConfig
    , Timeline(..)
    , activityResponse
    , calendarDisplay
    , controlsView
    , init
    , queryChanged
    , reportFailed
    , reportLoaded
    , reportRequest
    , requestReport
    , transaction
    , update
    , view
    )

import Api.Analytics as AnalyticsApi
import Api.Error exposing (ApiError)
import Data.Analytics as Analytics
    exposing
        ( ActivityRange(..)
        , CategoryBreakdown
        , TimelinePoint
        )
import Data.Calendar as Calendar
import Data.Granularity exposing (Granularity(..))
import Data.Time as Time exposing (Depth(..), FiscalYearStartMonth(..), Month(..), Period(..))
import Data.Transaction exposing (Transaction)
import Element exposing (Element)
import Element.Font as Font
import Ui.DesignTokens as Tokens
import Ui.Feedback as Feedback
import Ui.Input
import Ui.Page as Page
import Ui.Surface as Surface
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import Ui.Viewport as Viewport exposing (Viewport)
import View.Analytics as AnalyticsView exposing (LegendState(..))



-- MODEL


type Model
    = Model ModelData


type alias ModelData =
    { granularity : Granularity
    , depth : Depth
    , calendarDisplay : AnalyticsView.CalendarDisplay
    , period : Maybe Period
    , calendarMonth : Maybe Month
    , timeline : List TimelinePoint
    , positive : SideState
    , negative : SideState
    , mode : Mode
    , fiscalYearStartMonth : FiscalYearStartMonth
    , nextRequestToken : Int
    , activeReportToken : Maybe Int
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
    | ActivityLoading (Maybe Int) (Maybe Analytics.ActivityResponse)
    | ActivityReady Analytics.ActivityResponse
    | ActivityFailed ApiError (Maybe Analytics.ActivityResponse)


type Mode
    = Exploring (Maybe Selection)
    | ViewingTransaction Selection Transaction


type Timeline
    = TimelineAvailable (List TimelinePoint)
    | TimelineUnavailable String (List TimelinePoint)


type alias Report =
    { displayCommodity : String
    , timeline : Timeline
    , positiveBreakdown : CategoryBreakdown
    , negativeBreakdown : CategoryBreakdown
    , warnings : List String
    }


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
    , showCalendarDisplayControl : Bool
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
        , calendarDisplay = AnalyticsView.ByCategory
        , period = Nothing
        , calendarMonth = Nothing
        , timeline = []
        , positive = initialSide
        , negative = initialSide
        , mode = Exploring Nothing
        , fiscalYearStartMonth = FiscalYearStartMonth 4
        , nextRequestToken = 0
        , activeReportToken = Nothing
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
    | CalendarDisplaySelected AnalyticsView.CalendarDisplay
    | OtherToggled Side
    | LegendToggled Side
    | LegendCategoryHighlighted Side Int
    | DonutSliceClicked Side Int
    | ActivityLoaded Side Int (Result ApiError Analytics.ActivityResponse)
    | BucketSelected Side Analytics.ActivityBucket
    | YearBucketSelected Side Analytics.ActivityBucket
    | TransactionSelected Side Analytics.ActivityBucket Transaction
    | Back


type Effect
    = FetchReport Int AnalyticsApi.AnalyticsRequest
    | FetchActivity Side Int AnalyticsApi.ActivityRequest


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
                            calendarMonthForPeriod
                                data.fiscalYearStartMonth
                                data.granularity
                                selectedPeriod
                                point
                                |> Just

                        next =
                            selectPeriod selectedPeriod selectedMonth data
                    in
                    withReportAndActivities context next

                _ ->
                    ( Model data, [] )

        PeriodChanged offset ->
            case data.period of
                Just currentPeriod ->
                    let
                        adjacentPoint =
                            timelinePointAtOffset
                                data.granularity
                                offset
                                currentPeriod
                                data.timeline
                    in
                    case adjacentPoint of
                        Just point ->
                            case periodFromString data.granularity point.period of
                                Just period ->
                                    selectPeriod
                                        period
                                        (Just
                                            (calendarMonthForPeriod
                                                data.fiscalYearStartMonth
                                                data.granularity
                                                period
                                                point
                                            )
                                        )
                                        data
                                        |> withReportAndActivities context

                                Nothing ->
                                    ( Model data, [] )

                        Nothing ->
                            ( Model data, [] )

                _ ->
                    ( Model data, [] )

        DepthSelected requestedDepth ->
            case Time.depthFromInt requestedDepth of
                Nothing ->
                    ( Model data, [] )

                Just depth ->
                    withReportAndActivities context { data | depth = depth }

        CalendarDisplaySelected display ->
            ( Model
                { data
                    | calendarDisplay = display
                    , positive = clearCalendarHighlight data.positive
                    , negative = clearCalendarHighlight data.negative
                }
            , []
            )

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

        ActivityLoaded side token result ->
            let
                sideState =
                    sideFor side data
            in
            case sideState.activity of
                ActivityLoading (Just activeToken) stale ->
                    if token /= activeToken then
                        ( Model data, [] )

                    else
                        let
                            remote =
                                case result of
                                    Ok response ->
                                        ActivityReady response

                                    Err error ->
                                        ActivityFailed error stale

                            fiscalYearStartMonth =
                                result
                                    |> Result.toMaybe
                                    |> Maybe.andThen
                                        (Time.fiscalYearStartMonthFromInt
                                            << .fiscalYearStartMonth
                                        )
                                    |> Maybe.withDefault data.fiscalYearStartMonth
                        in
                        ( Model
                            (setSide side
                                { sideState | activity = remote }
                                { data | fiscalYearStartMonth = fiscalYearStartMonth }
                            )
                        , []
                        )

                _ ->
                    ( Model data, [] )

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

        TransactionSelected side bucket selectedTransaction ->
            ( Model
                { data
                    | mode = ViewingTransaction (Selection side bucket) selectedTransaction
                }
            , []
            )

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


reportLoaded : Int -> Context -> Report -> Model -> Maybe ( Model, List Effect )
reportLoaded token context report (Model data) =
    if data.activeReportToken /= Just token then
        Nothing

    else
        let
            points =
                timelinePoints report.timeline

            period =
                syncedPeriod data.period points data.granularity

            pointMonth =
                Maybe.map2
                    (calendarMonthForPeriod data.fiscalYearStartMonth data.granularity)
                    period
                    (period |> Maybe.andThen (Time.periodToString >> pointFor points))

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
                    , activeReportToken = Nothing
                }
        in
        Just (withActivities context next)


reportFailed : Int -> Model -> Maybe Model
reportFailed token (Model data) =
    if data.activeReportToken == Just token then
        Just (Model { data | activeReportToken = Nothing })

    else
        Nothing


reportRequest : String -> Model -> AnalyticsApi.AnalyticsRequest
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
    let
        token =
            data.nextRequestToken + 1

        next =
            { data | nextRequestToken = token, activeReportToken = Just token }
    in
    ( Model next, [ FetchReport token (reportRequest context.query (Model next)) ] )


requestReport : Context -> Model -> ( Model, List Effect )
requestReport context (Model data) =
    withReport context data


withReportAndActivities : Context -> ModelData -> ( Model, List Effect )
withReportAndActivities context data =
    let
        ( Model reportData, reportEffects ) =
            withReport context data

        ( Model next, activityCommands ) =
            withActivities context reportData
    in
    ( Model next, reportEffects ++ activityCommands )


withActivities : Context -> ModelData -> ( Model, List Effect )
withActivities context data =
    List.foldl (beginActivityRequest context) ( Model data, [] ) [ Positive, Negative ]


beginActivityRequest : Context -> Side -> ( Model, List Effect ) -> ( Model, List Effect )
beginActivityRequest context side ( Model data, effects ) =
    case activityRequest context side data of
        Nothing ->
            ( Model data, effects )

        Just request ->
            let
                token =
                    data.nextRequestToken + 1

                sideState =
                    sideFor side data

                next =
                    setSide side
                        { sideState
                            | activity =
                                ActivityLoading (Just token) (activityValue sideState.activity)
                        }
                        { data | nextRequestToken = token }
            in
            ( Model next, effects ++ [ FetchActivity side token request ] )


activityRequest : Context -> Side -> ModelData -> Maybe AnalyticsApi.ActivityRequest
activityRequest context side data =
    data.calendarMonth
        |> Maybe.map
            (\month ->
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
    Ui.Input.wideSegmentedControl theme
        [ { label = "M"
          , selected = data.granularity == Monthly
          , onPress = GranularitySelected Monthly
          }
        , { label = "Y"
          , selected = data.granularity == Yearly
          , onPress = GranularitySelected Yearly
          }
        ]



-- VIEW


view : Theme -> Viewport -> Config -> Report -> Model -> Element Msg
view theme viewport config report (Model data) =
    let
        layout =
            Viewport.contentLayout viewport

        chartWidth =
            let
                maxContainerWidth =
                    1280 - 2 * Viewport.contentPadding - 40
            in
            min maxContainerWidth (max 320 (Viewport.usableContentWidth viewport - 40))

        activityColumns =
            let
                cardWidth =
                    if Viewport.isCompact layout then
                        Viewport.usableContentWidth viewport

                    else
                        (Viewport.usableContentWidth viewport - 16) // 2
            in
            if cardWidth < 360 then
                3

            else if cardWidth < 540 then
                4

            else
                6
    in
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ]
        [ Typography.sectionTitle theme config.timelineSectionTitle
        , timelineView theme chartWidth config report data
        , AnalyticsView.warningList theme report.warnings
        , compositionHeader theme layout data
        , breakdownsView theme layout config report data
        , activityViews theme layout activityColumns config report data
        ]


compositionHeader : Theme -> Viewport.ContentLayout -> ModelData -> Element Msg
compositionHeader theme layout data =
    let
        container =
            if Viewport.isCompact layout then
                Element.wrappedRow

            else
                Element.row
    in
    container [ Element.width Element.fill, Element.spacing Tokens.space12, Element.alignTop ]
        [ Element.el [ Element.width Element.fill ]
            (Typography.sectionTitle theme "Composition and activity")
        , Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
            [ Element.row [ Element.width Element.fill, Element.spacing Tokens.space8 ]
                [ Element.el
                    [ Element.width (Element.fillPortion Tokens.layoutPortionSingle)
                    , Element.alignLeft
                    ]
                    (Typography.mutedBody theme "Period")
                , Element.row [ Element.width (Element.fillPortion Tokens.layoutPortionDouble) ]
                    [ Element.el [ Element.width Element.fill ] Element.none
                    , Page.periodNavigation theme
                        { label = selectedPeriodLabel data
                        , labelKind =
                            case data.granularity of
                                Monthly ->
                                    Page.MonthPeriodLabel

                                Yearly ->
                                    Page.FiscalYearPeriodLabel
                        , onPrevious = PeriodChanged -1
                        , onNext = PeriodChanged 1
                        }
                    ]
                ]
            , Element.row [ Element.width Element.fill, Element.spacing Tokens.space10 ]
                [ Element.el
                    [ Element.width (Element.fillPortion Tokens.layoutPortionSingle)
                    , Element.alignLeft
                    ]
                    (Typography.mutedBody theme "Account depth")
                , Element.row [ Element.width (Element.fillPortion Tokens.layoutPortionDouble) ]
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
            Surface.card theme
                [ Typography.subsectionTitle theme config.timelineTitle
                , AnalyticsView.positiveNegativeTimelineChart theme
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
            Feedback.errorBanner theme message


breakdownsView : Theme -> Viewport.ContentLayout -> Config -> Report -> ModelData -> Element Msg
breakdownsView theme layout config report data =
    responsiveGrid layout
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
    -> CategoryBreakdown
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


activityViews :
    Theme
    -> Viewport.ContentLayout
    -> Int
    -> Config
    -> Report
    -> ModelData
    -> Element Msg
activityViews theme layout columns config report data =
    let
        maxLegendItems =
            legendCapacity layout
    in
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space12 ]
        [ if config.showCalendarDisplayControl then
            calendarDisplayControl theme data.calendarDisplay

          else
            Element.none
        , responsiveGrid layout
            [ activityView
                theme
                columns
                maxLegendItems
                data.granularity
                config.positive
                Positive
                report.displayCommodity
                data
            , activityView
                theme
                columns
                maxLegendItems
                data.granularity
                config.negative
                Negative
                report.displayCommodity
                data
            ]
        ]


calendarDisplayControl : Theme -> AnalyticsView.CalendarDisplay -> Element Msg
calendarDisplayControl theme selected =
    Element.wrappedRow [ Element.width Element.fill, Element.spacing Tokens.space10 ]
        [ Typography.mutedBody theme "Show calendar by"
        , Ui.Input.segmentedControl theme
            [ Ui.Input.segmentedOption theme
                { label = "Category"
                , selected = selected == AnalyticsView.ByCategory
                , onPress = CalendarDisplaySelected AnalyticsView.ByCategory
                }
            , Ui.Input.segmentedOption theme
                { label = "Movement"
                , selected = selected == AnalyticsView.ByMovement
                , onPress = CalendarDisplaySelected AnalyticsView.ByMovement
                }
            ]
        ]


legendCapacity : Viewport.ContentLayout -> Int
legendCapacity layout =
    case layout of
        Viewport.Compact ->
            4

        Viewport.Medium ->
            2

        Viewport.Wide ->
            3


activityView :
    Theme
    -> Int
    -> Int
    -> Granularity
    -> SideConfig
    -> Side
    -> String
    -> ModelData
    -> Element Msg
activityView theme columns maxLegendItems granularity config side commodity data =
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
                , calendarDisplay = data.calendarDisplay
                , response = activityValue sideState.activity
                , selectedBucket = selectedBucketFor side data.mode
                , onSelect = BucketSelected side
                , onTransaction = TransactionSelected side
                , legendState = sideState.legendState
                , onLegendToggle = LegendToggled side
                , highlightedCategory = sideState.calendarHighlight
                , onCategoryHighlight = LegendCategoryHighlighted side
                , maxVisible = maxLegendItems
                }

        Yearly ->
            AnalyticsView.activityYear theme
                { title = config.yearActivityTitle
                , commodity = commodity
                , tone = config.tone
                , calendarDisplay = data.calendarDisplay
                , response = activityValue sideState.activity
                , selectedBucket = sideState.selectedYearBucket
                , onSelect = YearBucketSelected side
                , onTransaction = TransactionSelected side
                , columns = columns
                , legendState = sideState.legendState
                , onLegendToggle = LegendToggled side
                , highlightedCategory = sideState.calendarHighlight
                , onCategoryHighlight = LegendCategoryHighlighted side
                , maxVisible = maxLegendItems
                }


transaction : Model -> Maybe Transaction
transaction (Model data) =
    case data.mode of
        Exploring _ ->
            Nothing

        ViewingTransaction _ selectedTransaction ->
            Just selectedTransaction


activityResponse : Side -> Model -> Maybe Analytics.ActivityResponse
activityResponse side (Model data) =
    activityValue (sideFor side data).activity


calendarDisplay : Model -> AnalyticsView.CalendarDisplay
calendarDisplay (Model data) =
    data.calendarDisplay


beginSideLoading : SideState -> SideState
beginSideLoading sideState =
    { sideState
        | activity = beginActivityLoading sideState.activity
        , selectedYearBucket = Nothing
        , legendState = LegendCollapsed
        , donutHighlight = Nothing
        , calendarHighlight = Nothing
    }


clearCalendarHighlight : SideState -> SideState
clearCalendarHighlight sideState =
    { sideState | calendarHighlight = Nothing, legendState = LegendCollapsed }


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

        ActivityLoading _ stale ->
            stale

        ActivityReady response ->
            Just response

        ActivityFailed _ stale ->
            stale


beginActivityLoading : ActivityState -> ActivityState
beginActivityLoading activity =
    ActivityLoading Nothing (activityValue activity)


timelinePoints : Timeline -> List TimelinePoint
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


timelinePointAtOffset :
    Granularity
    -> Int
    -> Period
    -> List TimelinePoint
    -> Maybe TimelinePoint
timelinePointAtOffset granularity offset currentPeriod points =
    points
        |> List.indexedMap Tuple.pair
        |> List.filter (Tuple.second >> periodMatches granularity currentPeriod)
        |> List.head
        |> Maybe.andThen
            (\( currentIndex, _ ) ->
                let
                    targetIndex =
                        currentIndex + offset
                in
                if targetIndex < 0 then
                    Nothing

                else
                    points
                        |> List.drop targetIndex
                        |> List.head
            )


calendarMonthForPeriod :
    FiscalYearStartMonth
    -> Granularity
    -> Period
    -> TimelinePoint
    -> Month
calendarMonthForPeriod fiscalYearStartMonth granularity period point =
    case ( granularity, period ) of
        ( Yearly, YearPeriod year ) ->
            Month
                { year = year
                , month = Time.fiscalYearStartMonthValue fiscalYearStartMonth
                }

        _ ->
            Time.monthFromDate point.startDate


pointFor : List TimelinePoint -> String -> Maybe TimelinePoint
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


responsiveGrid : Viewport.ContentLayout -> List (Element msg) -> Element msg
responsiveGrid layout items =
    if Viewport.isCompact layout then
        Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ] items

    else
        Element.row [ Element.width Element.fill, Element.spacing Tokens.space16 ] items


chartLegend : Theme -> List ( Element.Color, String ) -> Element msg
chartLegend theme items =
    Element.wrappedRow [ Element.spacing Tokens.space14 ]
        (List.map
            (\( color, label ) ->
                Element.row [ Element.spacing Tokens.space5 ]
                    [ Surface.accentDot color
                    , Element.el [ Font.color theme.textMuted ] (Element.text label)
                    ]
            )
            items
        )
