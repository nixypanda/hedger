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

import Api.Analytics as Analytics exposing (ActivityRange(..), Granularity(..))
import Api.Error as ApiError
import Api.Transaction exposing (Transaction)
import Data.Calendar as Calendar
import Data.Date as Date
import Element exposing (Device, Element)
import Element.Font as Font
import RemoteData exposing (RemoteData(..))
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
    , selectedTimeline : Maybe String
    , timeline : List Analytics.TimelinePoint
    , positive : SideState
    , negative : SideState
    , mode : Mode
    , fiscalYearStartMonth : Int
    }


type alias SideState =
    { period : Maybe String
    , depth : Depth
    , calendarMonth : Maybe CalendarMonth
    , activity : RemoteData Analytics.ActivityResponse
    , otherExpanded : Bool
    }


type Depth
    = Depth Int


type CalendarMonth
    = CalendarMonth String


type Mode
    = Exploring Selection
    | ViewingTransaction Selection Transaction


type Selection
    = NoSelection
    | SelectedBucket Side Analytics.ActivityBucket


type Side
    = Positive
    | Negative


type alias Context =
    { config : Config
    , query : String
    }


type alias Config =
    { timelineTitle : String
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


type alias Report =
    { displayCommodity : String
    , timeline : Timeline
    , positiveBreakdown : Analytics.CategoryBreakdown
    , negativeBreakdown : Analytics.CategoryBreakdown
    , warnings : List String
    }


type Timeline
    = TimelineAvailable (List Analytics.TimelinePoint)
    | TimelineUnavailable String (List Analytics.TimelinePoint)


init : Model
init =
    Model
        { granularity = Monthly
        , selectedTimeline = Nothing
        , timeline = []
        , positive = initialSide
        , negative = initialSide
        , mode = Exploring NoSelection
        , fiscalYearStartMonth = 4
        }


initialSide : SideState
initialSide =
    { period = Nothing
    , depth = Depth 2
    , calendarMonth = Nothing
    , activity = NotAsked
    , otherExpanded = False
    }



-- UPDATE


type Msg
    = GranularitySelected Granularity
    | TimelineSelected String
    | BreakdownPeriodChanged Side Int
    | DepthSelected Side Int
    | OtherToggled Side
    | CalendarPeriodChanged Side Int
    | ActivityLoaded Side (Result ApiError.ApiError Analytics.ActivityResponse)
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
                        , mode = Exploring NoSelection
                    }
            in
            withReportAndActivities context next

        TimelineSelected period ->
            if String.isEmpty period then
                ( Model data, [] )

            else
                let
                    selectedMonth =
                        pointFor data.timeline period
                            |> Maybe.map (.startDate >> Date.toString >> String.left 7 >> CalendarMonth)

                    next =
                        { data
                            | selectedTimeline = Just period
                            , positive = selectPeriod period selectedMonth data.positive
                            , negative = selectPeriod period selectedMonth data.negative
                            , mode = Exploring NoSelection
                        }
                in
                withReport context next

        BreakdownPeriodChanged side offset ->
            let
                sideState =
                    sideFor side data

                nextSide =
                    { sideState
                        | period = Maybe.map (Calendar.shiftPeriod data.granularity offset) sideState.period
                    }
            in
            withReport context (setSide side nextSide data)

        DepthSelected side requestedDepth ->
            if requestedDepth < 1 then
                ( Model data, [] )

            else
                let
                    sideState =
                        sideFor side data

                    next =
                        setSide side { sideState | depth = Depth requestedDepth } data
                in
                withReport context next

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

                Just (CalendarMonth currentMonth) ->
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
                                | calendarMonth =
                                    Just (CalendarMonth (Calendar.shiftMonth distance currentMonth))
                                , activity = RemoteData.beginLoading sideState.activity
                            }

                        next =
                            setSide side nextSide { data | mode = Exploring NoSelection }
                    in
                    ( Model next, activityEffect context side next |> maybeToList )

        ActivityLoaded side result ->
            let
                previous =
                    (sideFor side data).activity

                remote =
                    case result of
                        Ok response ->
                            Success response

                        Err error ->
                            Failure error (RemoteData.value previous)

                sideState =
                    sideFor side data

                fiscalYearStartMonth =
                    result
                        |> Result.toMaybe
                        |> Maybe.map .fiscalYearStartMonth
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
                        SelectedBucket selectedSide selectedBucket ->
                            if selectedSide == side && selectedBucket.key == bucket.key then
                                NoSelection

                            else
                                SelectedBucket side bucket

                        NoSelection ->
                            SelectedBucket side bucket
            in
            ( Model { data | mode = Exploring nextSelection }, [] )

        TransactionSelected selectedTransaction ->
            ( Model { data | mode = ViewingTransaction (selectionFor data.mode) selectedTransaction }, [] )

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
                , mode = Exploring NoSelection
            }
    in
    withReport context next


reportLoaded : Context -> Report -> Model -> ( Model, List Effect )
reportLoaded context report (Model data) =
    let
        points =
            timelinePoints report.timeline

        selectedTimeline =
            syncedPeriod data.selectedTimeline points

        selectedMonth =
            selectedTimeline
                |> Maybe.andThen (pointFor points)
                |> orElse (List.reverse points |> List.head)
                |> Maybe.map (.startDate >> Date.toString >> String.left 7 >> CalendarMonth)
                |> orElse (Just (CalendarMonth "1970-01"))

        updateSide breakdown sideState =
            { sideState
                | period = Just breakdown.period
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
    , positivePeriod = data.positive.period
    , negativePeriod = data.negative.period
    , positiveDepth = depthValue data.positive.depth
    , negativeDepth = depthValue data.negative.depth
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
            (\(CalendarMonth month) ->
                FetchActivity side
                    { kind = (sideConfigFor side context.config).activityKind
                    , range = activityRange data.granularity
                    , period =
                        if activityRange data.granularity == MonthActivity then
                            month

                        else
                            Calendar.fiscalPeriodForMonth data.fiscalYearStartMonth month
                    , query = context.query
                    , displayCommodity = Nothing
                    , depth = depthValue sideState.depth
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
        [ timelineView theme chartWidth config report data
        , AnalyticsView.warningList theme report.warnings
        , breakdownsView theme device config report data
        , Typography.sectionTitle theme "Activity"
        , activityViews theme device config report data
        ]


timelineView : Theme -> Int -> Config -> Report -> ModelData -> Element Msg
timelineView theme chartWidth config report data =
    case report.timeline of
        TimelineAvailable points ->
            Ui.card theme
                [ Typography.sectionTitle theme config.timelineTitle
                , AnalyticsView.timelineChart theme
                    { commodity = report.displayCommodity
                    , points = points
                    , selected = data.selectedTimeline
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
        , depth = depthValue sideState.depth
        , onDepth = DepthSelected side
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
                |> Maybe.map (\(CalendarMonth value) -> value)
                |> Maybe.withDefault ""
    in
    case granularity of
        Monthly ->
            AnalyticsView.activityMonth theme
                { title = config.monthActivityTitle
                , period = month
                , commodity = commodity
                , tone = config.tone
                , response = RemoteData.value sideState.activity
                , selectedBucket = selectedBucketFor side data.mode
                , onPrevious = CalendarPeriodChanged side -1
                , onNext = CalendarPeriodChanged side 1
                , onSelect = BucketSelected side
                , onTransaction = TransactionSelected
                }

        Yearly ->
            AnalyticsView.activityYear theme
                { title = config.yearActivityTitle
                , period = Calendar.fiscalPeriodForMonth data.fiscalYearStartMonth month
                , commodity = commodity
                , tone = config.tone
                , response = RemoteData.value sideState.activity
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
        , activity = RemoteData.beginLoading sideState.activity
    }


selectPeriod : String -> Maybe CalendarMonth -> SideState -> SideState
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


selectionFor : Mode -> Selection
selectionFor mode =
    case mode of
        Exploring selection ->
            selection

        ViewingTransaction selection _ ->
            selection


selectedBucketFor : Side -> Mode -> Maybe Analytics.ActivityBucket
selectedBucketFor side mode =
    case selectionFor mode of
        SelectedBucket selectedSide bucket ->
            if selectedSide == side then
                Just bucket

            else
                Nothing

        NoSelection ->
            Nothing


depthValue : Depth -> Int
depthValue (Depth value) =
    value


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


syncedPeriod : Maybe String -> List { point | period : String } -> Maybe String
syncedPeriod requested points =
    case requested of
        Just period ->
            if List.any (.period >> (==) period) points then
                Just period

            else
                List.reverse points |> List.head |> Maybe.map .period

        Nothing ->
            List.reverse points |> List.head |> Maybe.map .period


pointFor : List Analytics.TimelinePoint -> String -> Maybe Analytics.TimelinePoint
pointFor points period =
    List.filter (.period >> (==) period) points |> List.head


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
