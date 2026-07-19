module Page.NetWorth exposing (Model, Msg(..), init, update, view)

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


type Model
    = Model ModelData


type alias ModelData =
    { response : RemoteData Analytics.NetWorthResponse
    , granularity : Granularity
    , selectedTimeline : Maybe String
    , selectedPerformance : Maybe String
    , assetPeriod : Maybe String
    , liabilityPeriod : Maybe String
    , assetDepth : Int
    , liabilityDepth : Int
    , activityRange : ActivityRange
    , assetCalendarMonth : String
    , liabilityCalendarMonth : String
    , assetActivity : RemoteData Analytics.ActivityResponse
    , liabilityActivity : RemoteData Analytics.ActivityResponse
    , selectedBucket : Maybe Analytics.ActivityBucket
    , selectedSide : Maybe Side
    , selectedTransaction : Maybe Transaction
    , fiscalYearStartMonth : Int
    , assetOtherExpanded : Bool
    , liabilityOtherExpanded : Bool
    }


type Side
    = AssetSide
    | LiabilitySide


type Msg
    = Refresh
    | ResponseLoaded (Result ApiError.ApiError Analytics.NetWorthResponse)
    | GranularitySelected Granularity
    | TimelineSelected String
    | PerformanceSelected String
    | BreakdownPeriodChanged Side Int
    | DepthSelected Side Int
    | OtherToggled Side
    | CalendarPeriodChanged Side Int
    | AssetActivityLoaded (Result ApiError.ApiError Analytics.ActivityResponse)
    | LiabilityActivityLoaded (Result ApiError.ApiError Analytics.ActivityResponse)
    | BucketSelected Side Analytics.ActivityBucket
    | TransactionSelected Transaction
    | Back


init : ( Model, Cmd Msg )
init =
    let
        data =
            { response = Loading Nothing
            , granularity = Monthly
            , selectedTimeline = Nothing
            , selectedPerformance = Nothing
            , assetPeriod = Nothing
            , liabilityPeriod = Nothing
            , assetDepth = 2
            , liabilityDepth = 2
            , activityRange = MonthActivity
            , assetCalendarMonth = ""
            , liabilityCalendarMonth = ""
            , assetActivity = NotAsked
            , liabilityActivity = NotAsked
            , selectedBucket = Nothing
            , selectedSide = Nothing
            , selectedTransaction = Nothing
            , fiscalYearStartMonth = 4
            , assetOtherExpanded = False
            , liabilityOtherExpanded = False
            }
    in
    ( Model data, fetchReport data )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model data) =
    case msg of
        Refresh ->
            let
                next =
                    { data | response = RemoteData.beginLoading data.response }
            in
            ( Model next, fetchReport next )

        ResponseLoaded result ->
            case result of
                Err error ->
                    ( Model { data | response = Failure error (RemoteData.value data.response) }, Cmd.none )

                Ok response ->
                    let
                        latest =
                            List.reverse response.timeline |> List.head

                        selectedTimeline =
                            syncedPeriod data.selectedTimeline response.timeline

                        selectedMonth =
                            case selectedTimeline |> Maybe.andThen (pointFor response) of
                                Just point ->
                                    point.startDate |> Date.toString |> String.left 7

                                Nothing ->
                                    latest
                                        |> Maybe.map (.startDate >> Date.toString >> String.left 7)
                                        |> Maybe.withDefault "1970-01"

                        next =
                            { data
                                | response = Success response
                                , selectedTimeline = selectedTimeline
                                , assetPeriod = Just response.assetsBreakdown.period
                                , liabilityPeriod = Just response.liabilitiesBreakdown.period
                                , assetCalendarMonth =
                                    if String.isEmpty data.assetCalendarMonth then
                                        selectedMonth

                                    else
                                        data.assetCalendarMonth
                                , liabilityCalendarMonth =
                                    if String.isEmpty data.liabilityCalendarMonth then
                                        selectedMonth

                                    else
                                        data.liabilityCalendarMonth
                            }
                    in
                    ( Model next, fetchActivities next )

        GranularitySelected granularity ->
            let
                next =
                    { data
                        | granularity = granularity
                        , selectedTimeline = Nothing
                        , selectedPerformance = Nothing
                        , assetPeriod = Nothing
                        , liabilityPeriod = Nothing
                        , selectedBucket = Nothing
                        , selectedSide = Nothing
                        , response = RemoteData.beginLoading data.response
                        , activityRange = granularityActivityRange granularity
                        , assetActivity = RemoteData.beginLoading data.assetActivity
                        , liabilityActivity = RemoteData.beginLoading data.liabilityActivity
                    }
            in
            ( Model next, Cmd.batch [ fetchReport next, fetchActivities next ] )

        TimelineSelected period ->
            let
                month =
                    RemoteData.value data.response
                        |> Maybe.andThen (\response -> pointFor response period)
                        |> Maybe.map (.startDate >> Date.toString >> String.left 7)
                        |> Maybe.withDefault data.assetCalendarMonth

                next =
                    { data
                        | selectedTimeline = Just period
                        , assetPeriod = Just period
                        , liabilityPeriod = Just period
                        , assetCalendarMonth = month
                        , liabilityCalendarMonth = month
                        , selectedBucket = Nothing
                        , response = RemoteData.beginLoading data.response
                    }
            in
            ( Model next, fetchReport next )

        PerformanceSelected period ->
            ( Model
                { data
                    | selectedPerformance =
                        if data.selectedPerformance == Just period then
                            Nothing

                        else
                            Just period
                }
            , Cmd.none
            )

        BreakdownPeriodChanged side offset ->
            let
                current =
                    periodForSide side data

                nextPeriod =
                    Maybe.map (Calendar.shiftPeriod data.granularity offset) current

                next =
                    setSidePeriod side nextPeriod { data | response = RemoteData.beginLoading data.response }
            in
            ( Model next, fetchReport next )

        DepthSelected side depth ->
            let
                next =
                    setSideDepth side depth { data | response = RemoteData.beginLoading data.response }
            in
            ( Model next, fetchReport next )

        OtherToggled side ->
            ( Model
                (if side == AssetSide then
                    { data | assetOtherExpanded = not data.assetOtherExpanded }

                 else
                    { data | liabilityOtherExpanded = not data.liabilityOtherExpanded }
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

        AssetActivityLoaded result ->
            activityReceived AssetSide result data

        LiabilityActivityLoaded result ->
            activityReceived LiabilitySide result data

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


activityReceived :
    Side
    -> Result ApiError.ApiError Analytics.ActivityResponse
    -> ModelData
    -> ( Model, Cmd Msg )
activityReceived side result data =
    let
        previous =
            activityFor side data

        remote =
            case result of
                Ok response ->
                    Success response

                Err error ->
                    Failure error (RemoteData.value previous)

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
    Api.getNetWorth
        { query = ""
        , granularity = data.granularity
        , displayCommodity = Nothing
        , positivePeriod = data.assetPeriod
        , negativePeriod = data.liabilityPeriod
        , positiveDepth = data.assetDepth
        , negativeDepth = data.liabilityDepth
        }
        ResponseLoaded


fetchActivities : ModelData -> Cmd Msg
fetchActivities data =
    Cmd.batch [ fetchActivity AssetSide data, fetchActivity LiabilitySide data ]


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
                if side == AssetSide then
                    Analytics.AssetActivity

                else
                    Analytics.LiabilityActivity
            , range = data.activityRange
            , period = period
            , query = ""
            , displayCommodity = Nothing
            , depth = depthFor side data
            }
    in
    Api.getActivity request
        (if side == AssetSide then
            AssetActivityLoaded

         else
            LiabilityActivityLoaded
        )


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
                , title = Typography.pageTitle theme "Net Worth"
                , subtitle = Just "Assets, liabilities, performance, and activity"
                , actions =
                    [ Ui.iconButton device
                        theme
                        { icon = Icon.Refresh, title = "Refresh", onPress = Refresh }
                    ]
                }
        , Ui.pageContainer device
            (granularityControl theme data.granularity :: dashboardContent theme viewport device data)
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
            , responsiveGrid device
                [ AnalyticsView.kpiCard theme
                    { title = "Net worth"
                    , commodity = response.displayCommodity
                    , kpi = response.netWorth
                    , color = theme.primary
                    , icon = Icon.TrendingUp
                    , decreaseIsPositive = False
                    }
                , AnalyticsView.kpiCard theme
                    { title = "Assets"
                    , commodity = response.displayCommodity
                    , kpi = response.assets
                    , color = theme.asset
                    , icon = Icon.Wallet
                    , decreaseIsPositive = False
                    }
                , AnalyticsView.kpiCard theme
                    { title = "Liabilities"
                    , commodity = response.displayCommodity
                    , kpi = response.liabilities
                    , color = theme.liability
                    , icon = Icon.Receipt
                    , decreaseIsPositive = True
                    }
                ]
            , Ui.card theme
                [ Element.row [ Element.width Element.fill ]
                    [ Typography.sectionTitle theme "Assets and liabilities" ]
                , AnalyticsView.timelineChart theme
                    { commodity = response.displayCommodity
                    , points = response.timeline
                    , selected = data.selectedTimeline
                    , onSelect = TimelineSelected
                    , availableWidth = chartWidth
                    , positiveLabel = "Assets"
                    , negativeLabel = "Liabilities"
                    , netLabel = "Net worth"
                    }
                , chartLegend theme
                    [ ( theme.positive, "Assets" )
                    , ( theme.negative, "Liabilities" )
                    , ( theme.primary, "Net worth" )
                    ]
                ]
            , AnalyticsView.warningList theme response.warnings
            , responsiveGrid device
                [ AnalyticsView.categoryCard theme
                    { title = "Assets"
                    , commodity = response.displayCommodity
                    , breakdown = response.assetsBreakdown
                    , palette = AnalyticsView.AssetPalette
                    , depth = data.assetDepth
                    , onDepth = DepthSelected AssetSide
                    , onPrevious = BreakdownPeriodChanged AssetSide -1
                    , onNext = BreakdownPeriodChanged AssetSide 1
                    , otherExpanded = data.assetOtherExpanded
                    , onOtherToggle = OtherToggled AssetSide
                    }
                , AnalyticsView.categoryCard theme
                    { title = "Liabilities"
                    , commodity = response.displayCommodity
                    , breakdown = response.liabilitiesBreakdown
                    , palette = AnalyticsView.LiabilityPalette
                    , depth = data.liabilityDepth
                    , onDepth = DepthSelected LiabilitySide
                    , onPrevious = BreakdownPeriodChanged LiabilitySide -1
                    , onNext = BreakdownPeriodChanged LiabilitySide 1
                    , otherExpanded = data.liabilityOtherExpanded
                    , onOtherToggle = OtherToggled LiabilitySide
                    }
                ]
            , Typography.sectionTitle theme "Activity"
            , activityViews theme viewport device data response
            , Ui.card theme
                [ Element.row [ Element.width Element.fill ]
                    [ Typography.sectionTitle theme "Net Investment vs Net Worth"
                    , if response.incompletePeriods > 0 then
                        Ui.statusBadge theme
                            Ui.StatusWarning
                            (String.fromInt response.incompletePeriods ++ " incomplete")

                      else
                        Element.none
                    ]
                , AnalyticsView.performanceChart theme
                    { commodity = response.displayCommodity
                    , points = response.performance
                    , selected = data.selectedPerformance
                    , onSelect = PerformanceSelected
                    , availableWidth = chartWidth
                    }
                , chartLegend theme
                    [ ( theme.positive, "Net investment" )
                    , ( theme.primary, "Net worth" )
                    , ( theme.positive, "Gain" )
                    , ( theme.negative, "Loss" )
                    ]
                ]
            ]


activityViews : Theme -> Viewport -> Device -> ModelData -> Analytics.NetWorthResponse -> Element Msg
activityViews theme _ device data response =
    case data.activityRange of
        MonthActivity ->
            responsiveGrid device
                [ AnalyticsView.activityMonth theme
                    { title = "Asset allocation"
                    , period = data.assetCalendarMonth
                    , commodity = response.displayCommodity
                    , tone = AnalyticsView.AssetTone
                    , response = RemoteData.value data.assetActivity
                    , selectedBucket = selectedBucketFor AssetSide data
                    , onPrevious = CalendarPeriodChanged AssetSide -1
                    , onNext = CalendarPeriodChanged AssetSide 1
                    , onSelect = BucketSelected AssetSide
                    , onTransaction = TransactionSelected
                    }
                , AnalyticsView.activityMonth theme
                    { title = "Liability activity"
                    , period = data.liabilityCalendarMonth
                    , commodity = response.displayCommodity
                    , tone = AnalyticsView.LiabilityTone
                    , response = RemoteData.value data.liabilityActivity
                    , selectedBucket = selectedBucketFor LiabilitySide data
                    , onPrevious = CalendarPeriodChanged LiabilitySide -1
                    , onNext = CalendarPeriodChanged LiabilitySide 1
                    , onSelect = BucketSelected LiabilitySide
                    , onTransaction = TransactionSelected
                    }
                ]

        YearActivity ->
            responsiveGrid device
                [ AnalyticsView.activityYear theme
                    { title = "Asset allocation"
                    , period =
                        Calendar.fiscalPeriodForMonth
                            data.fiscalYearStartMonth
                            data.assetCalendarMonth
                    , commodity = response.displayCommodity
                    , tone = AnalyticsView.AssetTone
                    , response = RemoteData.value data.assetActivity
                    , onPrevious = CalendarPeriodChanged AssetSide -1
                    , onNext = CalendarPeriodChanged AssetSide 1
                    }
                , AnalyticsView.activityYear theme
                    { title = "Liability activity"
                    , period =
                        Calendar.fiscalPeriodForMonth
                            data.fiscalYearStartMonth
                            data.liabilityCalendarMonth
                    , commodity = response.displayCommodity
                    , tone = AnalyticsView.LiabilityTone
                    , response = RemoteData.value data.liabilityActivity
                    , onPrevious = CalendarPeriodChanged LiabilitySide -1
                    , onNext = CalendarPeriodChanged LiabilitySide 1
                    }
                ]


remoteNotice :
    Theme
    -> RemoteData Analytics.NetWorthResponse
    -> Analytics.NetWorthResponse
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


pointFor : Analytics.NetWorthResponse -> String -> Maybe Analytics.TimelinePoint
pointFor response period =
    List.filter (.period >> (==) period) response.timeline |> List.head


periodForSide : Side -> ModelData -> Maybe String
periodForSide side data =
    if side == AssetSide then
        data.assetPeriod

    else
        data.liabilityPeriod


setSidePeriod : Side -> Maybe String -> ModelData -> ModelData
setSidePeriod side period data =
    if side == AssetSide then
        { data | assetPeriod = period }

    else
        { data | liabilityPeriod = period }


depthFor : Side -> ModelData -> Int
depthFor side data =
    if side == AssetSide then
        data.assetDepth

    else
        data.liabilityDepth


setSideDepth : Side -> Int -> ModelData -> ModelData
setSideDepth side depth data =
    if side == AssetSide then
        { data | assetDepth = depth }

    else
        { data | liabilityDepth = depth }


calendarMonthFor : Side -> ModelData -> String
calendarMonthFor side data =
    if side == AssetSide then
        data.assetCalendarMonth

    else
        data.liabilityCalendarMonth


setCalendarMonth : Side -> String -> ModelData -> ModelData
setCalendarMonth side month data =
    if side == AssetSide then
        { data | assetCalendarMonth = month, assetActivity = RemoteData.beginLoading data.assetActivity }

    else
        { data
            | liabilityCalendarMonth = month
            , liabilityActivity = RemoteData.beginLoading data.liabilityActivity
        }


activityFor : Side -> ModelData -> RemoteData Analytics.ActivityResponse
activityFor side data =
    if side == AssetSide then
        data.assetActivity

    else
        data.liabilityActivity


setActivity : Side -> RemoteData Analytics.ActivityResponse -> ModelData -> ModelData
setActivity side remote data =
    if side == AssetSide then
        { data | assetActivity = remote }

    else
        { data | liabilityActivity = remote }
