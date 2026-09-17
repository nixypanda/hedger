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
    , controlsView
    , init
    , queryChanged
    , reportLoaded
    , reportRequest
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
    , timeline : List TimelinePoint
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
    = FetchReport AnalyticsApi.AnalyticsRequest
    | FetchActivity Side AnalyticsApi.ActivityRequest


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
            }
    in
    ( Model next, activityEffects context next )


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
            Responsive.contentLayout viewport

        chartWidth =
            let
                maxContainerWidth =
                    1280 - 2 * Responsive.contentPadding - 40
            in
            min maxContainerWidth (max 320 (Responsive.usableContentWidth viewport - 40))

        activityColumns =
            let
                cardWidth =
                    if Responsive.isCompact layout then
                        Responsive.usableContentWidth viewport

                    else
                        (Responsive.usableContentWidth viewport - 16) // 2
            in
            if cardWidth < 360 then
                3

            else if cardWidth < 540 then
                4

            else
                6
    in
    Element.column [ Element.width Element.fill, Element.spacing 16 ]
        [ Typography.sectionTitle theme config.timelineSectionTitle
        , timelineView theme chartWidth config report data
        , AnalyticsView.warningList theme report.warnings
        , compositionHeader theme layout data
        , breakdownsView theme layout config report data
        , activityViews theme layout activityColumns config report data
        ]


compositionHeader : Theme -> Responsive.ContentLayout -> ModelData -> Element Msg
compositionHeader theme layout data =
    let
        container =
            if Responsive.isCompact layout then
                Element.wrappedRow

            else
                Element.row
    in
    container [ Element.width Element.fill, Element.spacing 12, Element.alignTop ]
        [ Element.el [ Element.width Element.fill ]
            (Typography.sectionTitle theme "Composition and activity")
        , Element.column [ Element.width Element.fill, Element.spacing 8 ]
            [ Element.row [ Element.width Element.fill, Element.spacing 8 ]
                [ Element.el [ Element.width (Element.fillPortion 1), Element.alignLeft ]
                    (Typography.mutedBody theme "Period")
                , Element.row [ Element.width (Element.fillPortion 2) ]
                    [ Element.el [ Element.width Element.fill ] Element.none
                    , Ui.periodNavigation theme
                        { label = selectedPeriodLabel data
                        , labelKind =
                            case data.granularity of
                                Monthly ->
                                    Ui.MonthPeriodLabel

                                Yearly ->
                                    Ui.FiscalYearPeriodLabel
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
            Ui.errorBanner theme message


breakdownsView : Theme -> Responsive.ContentLayout -> Config -> Report -> ModelData -> Element Msg
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
    -> Responsive.ContentLayout
    -> Int
    -> Config
    -> Report
    -> ModelData
    -> Element Msg
activityViews theme layout columns config report data =
    responsiveGrid layout
        [ activityView
            theme
            columns
            data.granularity
            config.positive
            Positive
            report.displayCommodity
            data
        , activityView
            theme
            columns
            data.granularity
            config.negative
            Negative
            report.displayCommodity
            data
        ]


activityView :
    Theme
    -> Int
    -> Granularity
    -> SideConfig
    -> Side
    -> String
    -> ModelData
    -> Element Msg
activityView theme columns granularity config side commodity data =
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
                , columns = columns
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


responsiveGrid : Responsive.ContentLayout -> List (Element msg) -> Element msg
responsiveGrid layout items =
    if Responsive.isCompact layout then
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
