module Page.NetWorth exposing (Model, Msg(..), init, update, view)

import Api.Client as Api
import Api.Error as ApiError exposing (ApiError)
import Data.Analytics as Analytics
import Data.Transaction exposing (Transaction)
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Page.Shared.Analysis as Analysis
import RemoteData exposing (RemoteData(..))
import State.Viewport exposing (Viewport)
import Ui
import Ui.DesignTokens as Tokens
import Ui.Icon as Icon
import Ui.Responsive as Responsive
import Ui.Skeleton as Skeleton
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import View.Analytics as AnalyticsView
import View.States as States
import View.Transaction as TransactionView



-- MODEL


type Model
    = Model ModelData


type alias ModelData =
    { response : RemoteData ApiError Analytics.NetWorthResponse
    , analysis : Analysis.Model
    , selectedPerformance : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    let
        ( analysis, effects ) =
            Analysis.requestReport analysisContext Analysis.init

        data =
            { response = Loading Nothing
            , analysis = analysis
            , selectedPerformance = Nothing
            }
    in
    ( Model data, performEffects effects )



-- UPDATE


type Msg
    = Refresh
    | ResponseLoaded Int (Result ApiError Analytics.NetWorthResponse)
    | AnalysisMsg Analysis.Msg
    | PerformanceSelected String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model data) =
    case msg of
        Refresh ->
            let
                ( analysis, effects ) =
                    Analysis.requestReport analysisContext data.analysis
            in
            ( Model
                { data
                    | response = RemoteData.beginLoading data.response
                    , analysis = analysis
                }
            , performEffects effects
            )

        ResponseLoaded token result ->
            case result of
                Err error ->
                    case Analysis.reportFailed token data.analysis of
                        Nothing ->
                            ( Model data, Cmd.none )

                        Just analysis ->
                            ( Model
                                { data
                                    | response =
                                        Failure error (RemoteData.value data.response)
                                    , analysis = analysis
                                }
                            , Cmd.none
                            )

                Ok loadedResponse ->
                    case
                        Analysis.reportLoaded
                            token
                            analysisContext
                            (analysisReport loadedResponse)
                            data.analysis
                    of
                        Nothing ->
                            ( Model data, Cmd.none )

                        Just ( analysis, effects ) ->
                            ( Model
                                { data
                                    | response = Success loadedResponse
                                    , analysis = analysis
                                }
                            , performEffects effects
                            )

        AnalysisMsg analysisMsg ->
            let
                ( analysis, effects ) =
                    Analysis.update analysisContext analysisMsg data.analysis

                response =
                    if List.any isReportEffect effects then
                        RemoteData.beginLoading data.response

                    else
                        data.response

                selectedPerformance =
                    case analysisMsg of
                        Analysis.GranularitySelected _ ->
                            Nothing

                        _ ->
                            data.selectedPerformance
            in
            ( Model
                { data
                    | analysis = analysis
                    , response = response
                    , selectedPerformance = selectedPerformance
                }
            , performEffects effects
            )

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


analysisContext : Analysis.Context
analysisContext =
    { config = analysisConfig
    , query = ""
    }


analysisConfig : Analysis.Config
analysisConfig =
    { timelineSectionTitle = "Balance over time"
    , timelineTitle = "Assets and liabilities"
    , netLabel = "Net worth"
    , showCalendarDisplayControl = False
    , positive =
        { label = "Assets"
        , monthActivityTitle = "Asset activity"
        , yearActivityTitle = "Asset activity"
        , activityKind = Analytics.AssetActivity
        , palette = AnalyticsView.AssetPalette
        , tone = AnalyticsView.AssetTone
        }
    , negative =
        { label = "Liabilities"
        , monthActivityTitle = "Liability activity"
        , yearActivityTitle = "Liability activity"
        , activityKind = Analytics.LiabilityActivity
        , palette = AnalyticsView.LiabilityPalette
        , tone = AnalyticsView.LiabilityTone
        }
    }


analysisReport : Analytics.NetWorthResponse -> Analysis.Report
analysisReport response =
    { displayCommodity = response.displayCommodity
    , timeline = Analysis.TimelineAvailable response.timeline
    , positiveBreakdown = response.assetsBreakdown
    , negativeBreakdown = response.liabilitiesBreakdown
    , warnings = response.warnings
    }


performEffects : List Analysis.Effect -> Cmd Msg
performEffects effects =
    effects |> List.map performEffect |> Cmd.batch


performEffect : Analysis.Effect -> Cmd Msg
performEffect effect =
    case effect of
        Analysis.FetchReport token request ->
            Api.getNetWorth request (ResponseLoaded token)

        Analysis.FetchActivity side token request ->
            Api.getActivity request (Analysis.ActivityLoaded side token >> AnalysisMsg)


isReportEffect : Analysis.Effect -> Bool
isReportEffect effect =
    case effect of
        Analysis.FetchReport _ _ ->
            True

        Analysis.FetchActivity _ _ _ ->
            False



-- VIEW


view : Theme -> Viewport -> Model -> Element Msg
view theme viewport (Model data) =
    case Analysis.transaction data.analysis of
        Just transaction ->
            transactionView theme transaction

        Nothing ->
            dashboard theme viewport data


transactionView : Theme -> Transaction -> Element Msg
transactionView theme transaction =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color theme.pageBg
        , Element.scrollbarY
        ]
        [ Ui.pageHeaderBand theme <|
            Ui.pageHeader theme
                { leading =
                    Just
                        (Ui.backButton theme
                            { onPress = AnalysisMsg Analysis.Back }
                        )
                , title = Typography.pageTitle theme "Transaction Detail"
                , subtitle = Nothing
                , actions = []
                }
        , Ui.pageContainer [ TransactionView.transactionDetail theme transaction ]
        ]


dashboard : Theme -> Viewport -> ModelData -> Element Msg
dashboard theme viewport data =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color theme.pageBg
        , Element.scrollbarY
        , Typography.uiClass "page-scroll analytics-page"
        ]
        [ Ui.pageHeaderBand theme <|
            Ui.pageHeader theme
                { leading = Nothing
                , title = Typography.pageTitle theme "Net Worth"
                , subtitle = Just "Assets, liabilities, performance, and activity"
                , actions =
                    [ Element.map AnalysisMsg (Analysis.controlsView theme data.analysis)
                    , Ui.iconButton theme
                        { icon = Icon.Refresh, title = "Refresh", onPress = Refresh }
                    ]
                }
        , Ui.pageContainer
            (dashboardContent theme viewport data)
        ]


dashboardContent : Theme -> Viewport -> ModelData -> List (Element Msg)
dashboardContent theme viewport data =
    let
        layout =
            Responsive.contentLayout viewport

        chartWidth =
            let
                maxContainerWidth =
                    1280 - 2 * Responsive.contentPadding - 40
            in
            min maxContainerWidth (max 320 (Responsive.usableContentWidth viewport - 40))
    in
    case RemoteData.value data.response of
        Nothing ->
            case data.response of
                Failure error _ ->
                    [ Ui.errorBanner theme (ApiError.message error)
                    , Ui.secondaryButton theme { label = "Retry", onPress = Refresh }
                    ]

                _ ->
                    [ netWorthSkeleton theme viewport ]

        Just response ->
            [ States.remoteNotice ApiError.message theme response.metadata.freshness data.response
            , responsiveGrid layout
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
            , Typography.sectionTitle theme "Performance"
            , Ui.card theme
                [ Element.row [ Element.width Element.fill ]
                    [ Typography.subsectionTitle theme "Net Investment vs Net Worth"
                    , if response.incompletePeriods > 0 then
                        Ui.statusBadge theme
                            Ui.StatusWarning
                            (String.fromInt response.incompletePeriods ++ " incomplete")

                      else
                        Element.none
                    ]
                , AnalyticsView.netWorthPerformanceChart theme
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
            , Element.map AnalysisMsg
                (Analysis.view
                    theme
                    viewport
                    analysisConfig
                    (analysisReport response)
                    data.analysis
                )
            ]


responsiveGrid : Responsive.ContentLayout -> List (Element msg) -> Element msg
responsiveGrid layout items =
    if Responsive.isCompact layout then
        Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ] items

    else
        Element.row [ Element.width Element.fill, Element.spacing Tokens.space16 ] items


netWorthSkeleton : Theme -> Viewport -> Element Msg
netWorthSkeleton theme viewport =
    let
        layout =
            Responsive.contentLayout viewport
    in
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ]
        [ responsiveGrid layout
            [ Skeleton.kpiCard theme
            , Skeleton.kpiCard theme
            , Skeleton.kpiCard theme
            ]
        , Typography.sectionTitle theme "Performance"
        , Skeleton.chartCard theme
        , Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ]
            [ Skeleton.chartCard theme
            , responsiveGrid layout
                [ Skeleton.chartCard theme
                , Skeleton.chartCard theme
                ]
            ]
        ]


chartLegend : Theme -> List ( Element.Color, String ) -> Element msg
chartLegend theme items =
    Element.wrappedRow [ Element.spacing Tokens.space14 ]
        (List.map
            (\( color, label ) ->
                Element.row [ Element.spacing Tokens.space5 ]
                    [ Ui.accentDot color
                    , Element.el [ Font.color theme.textMuted ] (Element.text label)
                    ]
            )
            items
        )
