module Page.Cashflow exposing (Model, Msg(..), init, update, view)

import Api.Analytics as Analytics exposing (ActivityRange(..), Granularity(..))
import Api.Client as Api
import Api.Error as ApiError
import Api.Transaction exposing (Transaction)
import Data.Calendar as Calendar
import Data.Date as Date
import Element exposing (Device, Element)
import Element.Background as Background
import Element.Font as Font
import Html.Attributes
import Page.Transactions as Transactions
import RemoteData exposing (RemoteData(..))
import State.Viewport exposing (Viewport)
import Ui
import Ui.Icon as Icon
import Ui.Responsive as Responsive
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import View.Analytics as AnalyticsView
import View.States as States



-- MODEL


type Model
    = Model ModelData


type alias ModelData =
    { response : RemoteData Analytics.CashflowResponse
    , draftQuery : String
    , appliedQuery : String
    , granularity : Granularity
    , selectedTimeline : Maybe String
    , incomePeriod : Maybe String
    , expensePeriod : Maybe String
    , incomeDepth : Int
    , expenseDepth : Int
    , activityRange : ActivityRange
    , incomeCalendarMonth : String
    , expenseCalendarMonth : String
    , incomeActivity : RemoteData Analytics.ActivityResponse
    , expenseActivity : RemoteData Analytics.ActivityResponse
    , selectedBucket : Maybe Analytics.ActivityBucket
    , selectedSide : Maybe Side
    , selectedTransaction : Maybe Transaction
    , fiscalYearStartMonth : Int
    , incomeOtherExpanded : Bool
    , expenseOtherExpanded : Bool
    }


init : ( Model, Cmd Msg )
init =
    let
        data =
            { response = Loading Nothing
            , draftQuery = ""
            , appliedQuery = ""
            , granularity = Monthly
            , selectedTimeline = Nothing
            , incomePeriod = Nothing
            , expensePeriod = Nothing
            , incomeDepth = 2
            , expenseDepth = 2
            , activityRange = MonthActivity
            , incomeCalendarMonth = ""
            , expenseCalendarMonth = ""
            , incomeActivity = NotAsked
            , expenseActivity = NotAsked
            , selectedBucket = Nothing
            , selectedSide = Nothing
            , selectedTransaction = Nothing
            , fiscalYearStartMonth = 4
            , incomeOtherExpanded = False
            , expenseOtherExpanded = False
            }
    in
    ( Model data, fetchReport data )


type Side
    = IncomeSide
    | ExpenseSide



-- UPDATE


type Msg
    = Refresh
    | DraftChanged String
    | ApplyQuery
    | ClearQuery
    | ResponseLoaded (Result ApiError.ApiError Analytics.CashflowResponse)
    | GranularitySelected Granularity
    | TimelineSelected String
    | BreakdownPeriodChanged Side Int
    | DepthSelected Side Int
    | OtherToggled Side
    | CalendarPeriodChanged Side Int
    | IncomeActivityLoaded (Result ApiError.ApiError Analytics.ActivityResponse)
    | ExpenseActivityLoaded (Result ApiError.ApiError Analytics.ActivityResponse)
    | BucketSelected Side Analytics.ActivityBucket
    | TransactionSelected Transaction
    | Back


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model data) =
    case msg of
        Refresh ->
            reload data

        DraftChanged draft ->
            ( Model { data | draftQuery = draft }, Cmd.none )

        ApplyQuery ->
            reload
                { data
                    | appliedQuery = String.trim data.draftQuery
                    , selectedTimeline = Nothing
                    , incomePeriod = Nothing
                    , expensePeriod = Nothing
                    , selectedBucket = Nothing
                }

        ClearQuery ->
            reload
                { data
                    | draftQuery = ""
                    , appliedQuery = ""
                    , selectedTimeline = Nothing
                    , incomePeriod = Nothing
                    , expensePeriod = Nothing
                    , selectedBucket = Nothing
                }

        ResponseLoaded result ->
            case result of
                Err error ->
                    ( Model { data | response = Failure error (RemoteData.value data.response) }, Cmd.none )

                Ok response ->
                    let
                        selectedTimeline =
                            syncedPeriod data.selectedTimeline response.timeline

                        selectedMonth =
                            case selectedTimeline |> Maybe.andThen (pointFor response) of
                                Just point ->
                                    point.startDate |> Date.toString |> String.left 7

                                Nothing ->
                                    response.timeline
                                        |> List.reverse
                                        |> List.head
                                        |> Maybe.map (.startDate >> Date.toString >> String.left 7)
                                        |> Maybe.withDefault "1970-01"

                        next =
                            { data
                                | response = Success response
                                , selectedTimeline = selectedTimeline
                                , incomePeriod = Just response.incomeBreakdown.period
                                , expensePeriod = Just response.expenseBreakdown.period
                                , incomeCalendarMonth =
                                    if String.isEmpty data.incomeCalendarMonth then
                                        selectedMonth

                                    else
                                        data.incomeCalendarMonth
                                , expenseCalendarMonth =
                                    if String.isEmpty data.expenseCalendarMonth then
                                        selectedMonth

                                    else
                                        data.expenseCalendarMonth
                            }
                    in
                    ( Model next, fetchActivities next )

        GranularitySelected granularity ->
            let
                next =
                    { data
                        | granularity = granularity
                        , activityRange = granularityActivityRange granularity
                        , selectedTimeline = Nothing
                        , incomePeriod = Nothing
                        , expensePeriod = Nothing
                        , selectedBucket = Nothing
                        , selectedSide = Nothing
                        , incomeActivity = RemoteData.beginLoading data.incomeActivity
                        , expenseActivity = RemoteData.beginLoading data.expenseActivity
                    }
            in
            ( Model { next | response = RemoteData.beginLoading data.response }
            , Cmd.batch [ fetchReport next, fetchActivities next ]
            )

        TimelineSelected period ->
            let
                month =
                    RemoteData.value data.response
                        |> Maybe.andThen (\response -> pointFor response period)
                        |> Maybe.map (.startDate >> Date.toString >> String.left 7)
                        |> Maybe.withDefault data.incomeCalendarMonth
            in
            reload
                { data
                    | selectedTimeline = Just period
                    , incomePeriod = Just period
                    , expensePeriod = Just period
                    , incomeCalendarMonth = month
                    , expenseCalendarMonth = month
                    , selectedBucket = Nothing
                }

        BreakdownPeriodChanged side offset ->
            let
                next =
                    setSidePeriod side
                        (Maybe.map (Calendar.shiftPeriod data.granularity offset) (periodFor side data))
                        data
            in
            reload next

        DepthSelected side depth ->
            reload (setDepth side depth data)

        OtherToggled side ->
            ( Model
                (if side == IncomeSide then
                    { data | incomeOtherExpanded = not data.incomeOtherExpanded }

                 else
                    { data | expenseOtherExpanded = not data.expenseOtherExpanded }
                )
            , Cmd.none
            )

        CalendarPeriodChanged side offset ->
            let
                distance =
                    if data.activityRange == MonthActivity then
                        offset

                    else
                        offset * 12

                next =
                    setCalendarMonth side
                        (Calendar.shiftMonth distance (calendarMonthFor side data))
                        { data | selectedBucket = Nothing }
            in
            ( Model next, fetchActivity side next )

        IncomeActivityLoaded result ->
            activityReceived IncomeSide result data

        ExpenseActivityLoaded result ->
            activityReceived ExpenseSide result data

        BucketSelected side bucket ->
            let
                deselect =
                    data.selectedSide
                        == Just side
                        && Maybe.map .key data.selectedBucket
                        == Just bucket.key
            in
            ( Model
                { data
                    | selectedBucket =
                        if deselect then
                            Nothing

                        else
                            Just bucket
                    , selectedSide =
                        if deselect then
                            Nothing

                        else
                            Just side
                }
            , Cmd.none
            )

        TransactionSelected transaction ->
            ( Model { data | selectedTransaction = Just transaction }, Cmd.none )

        Back ->
            ( Model { data | selectedTransaction = Nothing }, Cmd.none )


reload : ModelData -> ( Model, Cmd Msg )
reload data =
    let
        next =
            { data | response = RemoteData.beginLoading data.response }
    in
    ( Model next, fetchReport next )


activityReceived :
    Side
    -> Result ApiError.ApiError Analytics.ActivityResponse
    -> ModelData
    -> ( Model, Cmd Msg )
activityReceived side result data =
    let
        remote =
            case result of
                Ok response ->
                    Success response

                Err error ->
                    Failure error (RemoteData.value (activityFor side data))

        next =
            setActivity side remote data

        fiscal =
            result
                |> Result.toMaybe
                |> Maybe.map .fiscalYearStartMonth
                |> Maybe.withDefault data.fiscalYearStartMonth
    in
    ( Model { next | fiscalYearStartMonth = fiscal }, Cmd.none )


fetchReport : ModelData -> Cmd Msg
fetchReport data =
    Api.getCashflow
        { query = data.appliedQuery
        , granularity = data.granularity
        , displayCommodity = Nothing
        , positivePeriod = data.incomePeriod
        , negativePeriod = data.expensePeriod
        , positiveDepth = data.incomeDepth
        , negativeDepth = data.expenseDepth
        }
        ResponseLoaded


fetchActivities : ModelData -> Cmd Msg
fetchActivities data =
    Cmd.batch [ fetchActivity IncomeSide data, fetchActivity ExpenseSide data ]


fetchActivity : Side -> ModelData -> Cmd Msg
fetchActivity side data =
    let
        month =
            calendarMonthFor side data

        period =
            if data.activityRange == MonthActivity then
                month

            else
                Calendar.fiscalPeriodForMonth data.fiscalYearStartMonth month

        request =
            { kind =
                if side == IncomeSide then
                    Analytics.IncomeActivity

                else
                    Analytics.ExpenseActivity
            , range = data.activityRange
            , period = period
            , query = data.appliedQuery
            , displayCommodity = Nothing
            , depth = depthFor side data
            }
    in
    Api.getActivity request
        (if side == IncomeSide then
            IncomeActivityLoaded

         else
            ExpenseActivityLoaded
        )



-- VIEW


view : Theme -> Viewport -> Model -> Element Msg
view theme viewport (Model data) =
    let
        device =
            Element.classifyDevice viewport
    in
    case data.selectedTransaction of
        Just transaction ->
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Background.color theme.pageBg
                , Element.scrollbarY
                ]
                [ Ui.pageHeaderBand theme device <|
                    Ui.pageHeader theme
                        { leading = Just (Ui.ghostButton theme { label = "← Back", onPress = Back })
                        , title = Typography.pageTitle theme "Transaction Detail"
                        , subtitle = Nothing
                        , actions = []
                        }
                , Ui.pageContainer device [ Transactions.transactionDetail theme transaction ]
                ]

        Nothing ->
            dashboard theme viewport device data


dashboard : Theme -> Viewport -> Device -> ModelData -> Element Msg
dashboard theme viewport device data =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color theme.pageBg
        , Element.scrollbarY
        , Element.htmlAttribute (Html.Attributes.class "page-scroll analytics-page")
        ]
        [ Ui.pageHeaderBand theme device <|
            Ui.pageHeader theme
                { leading = Nothing
                , title = Typography.pageTitle theme "Cashflow"
                , subtitle = Just "Income, expenses, categories, and daily activity"
                , actions =
                    [ Ui.iconButton device
                        theme
                        { icon = Icon.Refresh, title = "Refresh", onPress = Refresh }
                    ]
                }
        , Ui.pageContainer device
            (granularityControl theme data.granularity
                :: queryPanel theme data
                :: dashboardContent theme viewport device data
            )
        ]


queryPanel : Theme -> ModelData -> Element Msg
queryPanel theme data =
    Ui.card theme
        [ Element.column [ Element.width Element.fill, Element.spacing 8 ]
            [ Ui.searchInput theme
                { label = "hledger query"
                , text = data.draftQuery
                , onChange = DraftChanged
                , extraAttrs =
                    [ Element.htmlAttribute (Html.Attributes.id "cashflow-query")
                    , Element.htmlAttribute (Html.Attributes.attribute "autocomplete" "off")
                    , Element.htmlAttribute
                        (Html.Attributes.attribute
                            "aria-describedby"
                            "cashflow-query-help cashflow-query-error"
                        )
                    ]
                }
            , Element.row [ Element.alignRight, Element.spacing 8 ]
                [ Ui.secondaryButton theme { label = "Clear", onPress = ClearQuery }
                , Ui.primaryButton theme { label = "Apply", onPress = ApplyQuery }
                ]
            ]
        , queryError data.response
            |> Maybe.map
                (ApiError.message
                    >> Ui.errorBanner theme
                    >> Element.el
                        [ Element.width Element.fill
                        , Element.htmlAttribute (Html.Attributes.id "cashflow-query-error")
                        , Element.htmlAttribute (Html.Attributes.attribute "role" "alert")
                        ]
                )
            |> Maybe.withDefault Element.none
        , Element.el [ Element.htmlAttribute (Html.Attributes.id "cashflow-query-help") ] <|
            Typography.mutedParagraph theme
                ("Filter this entire dashboard with an hledger query. Category, timeline, "
                    ++ "and activity results always use the same applied filter."
                )
        , Element.wrappedRow [ Element.width Element.fill, Element.spacing 8 ]
            (Typography.mutedCaption theme "Examples:"
                :: List.map
                    (\example ->
                        Ui.chip theme
                            { label = example
                            , selected = data.draftQuery == example
                            , onClick = Just (DraftChanged example)
                            }
                    )
                    [ "acct:expenses", "desc:grocery", "date:2026-01-01..2026-07-31", "cur:INR", "status:*" ]
            )
        ]


dashboardContent : Theme -> Viewport -> Device -> ModelData -> List (Element Msg)
dashboardContent theme viewport device data =
    let
        chartWidth =
            max 320 (Responsive.usableContentWidth viewport device - 40)
    in
    case RemoteData.value data.response of
        Nothing ->
            case data.response of
                Failure error _ ->
                    [ Ui.errorBanner theme (ApiError.message error)
                    , Ui.secondaryButton theme { label = "Retry", onPress = Refresh }
                    ]

                _ ->
                    [ Ui.skeletonLoading theme ]

        Just response ->
            [ remoteNotice theme data.response response
            , Typography.mutedBody theme
                (String.fromInt response.filteredCount
                    ++ " of "
                    ++ String.fromInt response.totalCount
                    ++ " transactions"
                )
            , if response.timelineAvailable then
                Ui.card theme
                    [ Typography.sectionTitle theme "Income and expenses"
                    , AnalyticsView.timelineChart theme
                        { commodity = response.displayCommodity
                        , points = response.timeline
                        , selected = data.selectedTimeline
                        , onSelect = TimelineSelected
                        , availableWidth = chartWidth
                        , positiveLabel = "Income"
                        , negativeLabel = "Expenses"
                        , netLabel = "Net"
                        }
                    , chartLegend theme
                        [ ( theme.positive, "Income" )
                        , ( theme.negative, "Expenses" )
                        , ( theme.primary, "Net" )
                        ]
                    ]

              else
                Ui.errorBanner theme
                    ("Cashflow is unavailable because at least one participating amount "
                        ++ "cannot be converted to the display commodity."
                    )
            , AnalyticsView.warningList theme response.warnings
            , responsiveGrid device
                [ AnalyticsView.categoryCard theme
                    { title = "Income"
                    , commodity = response.displayCommodity
                    , breakdown = response.incomeBreakdown
                    , palette = AnalyticsView.PositivePalette
                    , depth = data.incomeDepth
                    , onDepth = DepthSelected IncomeSide
                    , onPrevious = BreakdownPeriodChanged IncomeSide -1
                    , onNext = BreakdownPeriodChanged IncomeSide 1
                    , otherExpanded = data.incomeOtherExpanded
                    , onOtherToggle = OtherToggled IncomeSide
                    }
                , AnalyticsView.categoryCard theme
                    { title = "Expenses"
                    , commodity = response.displayCommodity
                    , breakdown = response.expenseBreakdown
                    , palette = AnalyticsView.NegativePalette
                    , depth = data.expenseDepth
                    , onDepth = DepthSelected ExpenseSide
                    , onPrevious = BreakdownPeriodChanged ExpenseSide -1
                    , onNext = BreakdownPeriodChanged ExpenseSide 1
                    , otherExpanded = data.expenseOtherExpanded
                    , onOtherToggle = OtherToggled ExpenseSide
                    }
                ]
            , Typography.sectionTitle theme "Activity"
            , activityViews theme device data response
            ]


activityViews : Theme -> Device -> ModelData -> Analytics.CashflowResponse -> Element Msg
activityViews theme device data response =
    case data.activityRange of
        MonthActivity ->
            responsiveGrid device
                [ AnalyticsView.activityMonth theme
                    { title = "Income calendar"
                    , period = data.incomeCalendarMonth
                    , commodity = response.displayCommodity
                    , tone = AnalyticsView.IncomeTone
                    , response = RemoteData.value data.incomeActivity
                    , selectedBucket = selectedBucketFor IncomeSide data
                    , onPrevious = CalendarPeriodChanged IncomeSide -1
                    , onNext = CalendarPeriodChanged IncomeSide 1
                    , onSelect = BucketSelected IncomeSide
                    , onTransaction = TransactionSelected
                    }
                , AnalyticsView.activityMonth theme
                    { title = "Expenses calendar"
                    , period = data.expenseCalendarMonth
                    , commodity = response.displayCommodity
                    , tone = AnalyticsView.ExpenseTone
                    , response = RemoteData.value data.expenseActivity
                    , selectedBucket = selectedBucketFor ExpenseSide data
                    , onPrevious = CalendarPeriodChanged ExpenseSide -1
                    , onNext = CalendarPeriodChanged ExpenseSide 1
                    , onSelect = BucketSelected ExpenseSide
                    , onTransaction = TransactionSelected
                    }
                ]

        YearActivity ->
            responsiveGrid device
                [ AnalyticsView.activityYear theme
                    { title = "Income activity"
                    , period =
                        Calendar.fiscalPeriodForMonth
                            data.fiscalYearStartMonth
                            data.incomeCalendarMonth
                    , commodity = response.displayCommodity
                    , tone = AnalyticsView.IncomeTone
                    , response = RemoteData.value data.incomeActivity
                    , onPrevious = CalendarPeriodChanged IncomeSide -1
                    , onNext = CalendarPeriodChanged IncomeSide 1
                    }
                , AnalyticsView.activityYear theme
                    { title = "Expense activity"
                    , period =
                        Calendar.fiscalPeriodForMonth
                            data.fiscalYearStartMonth
                            data.expenseCalendarMonth
                    , commodity = response.displayCommodity
                    , tone = AnalyticsView.ExpenseTone
                    , response = RemoteData.value data.expenseActivity
                    , onPrevious = CalendarPeriodChanged ExpenseSide -1
                    , onNext = CalendarPeriodChanged ExpenseSide 1
                    }
                ]


remoteNotice :
    Theme
    -> RemoteData Analytics.CashflowResponse
    -> Analytics.CashflowResponse
    -> Element Msg
remoteNotice theme remote response =
    Element.column [ Element.width Element.fill, Element.spacing 8 ]
        [ States.freshnessBanner theme response.metadata.freshness
        , case remote of
            Loading _ ->
                Ui.statusBadge theme Ui.StatusPrimary "Refreshing"

            Failure error _ ->
                Ui.errorBanner theme (ApiError.message error)

            _ ->
                Element.none
        ]


queryError : RemoteData value -> Maybe ApiError.ApiError
queryError remote =
    case remote of
        Failure error _ ->
            if List.member error.code [ "invalid_query", "unsupported_query_option", "query_too_long" ] then
                Just error

            else
                Nothing

        _ ->
            Nothing


granularityControl : Theme -> Granularity -> Element Msg
granularityControl theme selected =
    Ui.wideSegmentedControl theme
        [ { label = "M", selected = selected == Monthly, onPress = GranularitySelected Monthly }
        , { label = "Y", selected = selected == Yearly, onPress = GranularitySelected Yearly }
        ]


granularityActivityRange : Granularity -> ActivityRange
granularityActivityRange granularity =
    if granularity == Monthly then
        MonthActivity

    else
        YearActivity


responsiveGrid : Device -> List (Element msg) -> Element msg
responsiveGrid device items =
    if Ui.isSmall device then
        Element.column [ Element.width Element.fill, Element.spacing 16 ] items

    else
        Element.row [ Element.width Element.fill, Element.spacing 16 ] items


selectedBucketFor : Side -> ModelData -> Maybe Analytics.ActivityBucket
selectedBucketFor side data =
    if data.selectedSide == Just side then
        data.selectedBucket

    else
        Nothing


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


pointFor : Analytics.CashflowResponse -> String -> Maybe Analytics.TimelinePoint
pointFor response period =
    List.filter (.period >> (==) period) response.timeline |> List.head


periodFor : Side -> ModelData -> Maybe String
periodFor side data =
    if side == IncomeSide then
        data.incomePeriod

    else
        data.expensePeriod


setSidePeriod : Side -> Maybe String -> ModelData -> ModelData
setSidePeriod side period data =
    if side == IncomeSide then
        { data | incomePeriod = period }

    else
        { data | expensePeriod = period }


depthFor : Side -> ModelData -> Int
depthFor side data =
    if side == IncomeSide then
        data.incomeDepth

    else
        data.expenseDepth


setDepth : Side -> Int -> ModelData -> ModelData
setDepth side depth data =
    if side == IncomeSide then
        { data | incomeDepth = depth }

    else
        { data | expenseDepth = depth }


calendarMonthFor : Side -> ModelData -> String
calendarMonthFor side data =
    if side == IncomeSide then
        data.incomeCalendarMonth

    else
        data.expenseCalendarMonth


setCalendarMonth : Side -> String -> ModelData -> ModelData
setCalendarMonth side month data =
    if side == IncomeSide then
        { data | incomeCalendarMonth = month, incomeActivity = RemoteData.beginLoading data.incomeActivity }

    else
        { data
            | expenseCalendarMonth = month
            , expenseActivity = RemoteData.beginLoading data.expenseActivity
        }


activityFor : Side -> ModelData -> RemoteData Analytics.ActivityResponse
activityFor side data =
    if side == IncomeSide then
        data.incomeActivity

    else
        data.expenseActivity


setActivity : Side -> RemoteData Analytics.ActivityResponse -> ModelData -> ModelData
setActivity side remote data =
    if side == IncomeSide then
        { data | incomeActivity = remote }

    else
        { data | expenseActivity = remote }
