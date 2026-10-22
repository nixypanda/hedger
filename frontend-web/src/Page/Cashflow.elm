module Page.Cashflow exposing (Model, Msg(..), init, update, view)

import Api.Client as Api
import Api.Error as ApiError exposing (ApiError)
import Data.Analytics as Analytics
import Data.Transaction exposing (Transaction)
import Element exposing (Element)
import Element.Background as Background
import Page.Shared.Analysis as Analysis
import Page.Shared.QueryPanel as QueryPanel
import RemoteData exposing (RemoteData(..))
import Ui
import Ui.Button as Button
import Ui.Common as Common
import Ui.DesignTokens as Tokens
import Ui.Icon as Icon
import Ui.Skeleton as Skeleton
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import Ui.Viewport as Viewport exposing (Viewport)
import View.Analytics as AnalyticsView
import View.States as States
import View.Transaction as TransactionView



-- MODEL


type Model
    = Model ModelData


type alias ModelData =
    { response : RemoteData ApiError Analytics.CashflowResponse
    , draftQuery : String
    , appliedQuery : String
    , analysis : Analysis.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( analysis, effects ) =
            Analysis.requestReport (analysisContext "") Analysis.init

        data =
            { response = Loading Nothing
            , draftQuery = ""
            , appliedQuery = ""
            , analysis = analysis
            }
    in
    ( Model data, performEffects effects )



-- UPDATE


type Msg
    = Refresh
    | DraftChanged String
    | ApplyQuery
    | ClearQuery
    | ResponseLoaded Int (Result ApiError Analytics.CashflowResponse)
    | AnalysisMsg Analysis.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model data) =
    case msg of
        Refresh ->
            let
                ( analysis, effects ) =
                    Analysis.requestReport
                        (analysisContext data.appliedQuery)
                        data.analysis
            in
            ( Model
                { data
                    | response = RemoteData.beginLoading data.response
                    , analysis = analysis
                }
            , performEffects effects
            )

        DraftChanged draft ->
            ( Model { data | draftQuery = draft }, Cmd.none )

        ApplyQuery ->
            applyQuery (String.trim data.draftQuery) data

        ClearQuery ->
            applyQuery "" { data | draftQuery = "" }

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
                            (analysisContext data.appliedQuery)
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
                    Analysis.update (analysisContext data.appliedQuery) analysisMsg data.analysis

                response =
                    if List.any isReportEffect effects then
                        RemoteData.beginLoading data.response

                    else
                        data.response
            in
            ( Model { data | analysis = analysis, response = response }
            , performEffects effects
            )


applyQuery : String -> ModelData -> ( Model, Cmd Msg )
applyQuery query data =
    let
        context =
            analysisContext query

        ( analysis, effects ) =
            Analysis.queryChanged context data.analysis
    in
    ( Model
        { data
            | appliedQuery = query
            , analysis = analysis
            , response = RemoteData.beginLoading data.response
        }
    , performEffects effects
    )


analysisContext : String -> Analysis.Context
analysisContext query =
    { config = analysisConfig
    , query = query
    }


analysisConfig : Analysis.Config
analysisConfig =
    { timelineSectionTitle = "Cash flow over time"
    , timelineTitle = "Income and expenses"
    , netLabel = "Net"
    , showCalendarDisplayControl = True
    , positive =
        { label = "Income"
        , monthActivityTitle = "Income calendar"
        , yearActivityTitle = "Income activity"
        , activityKind = Analytics.IncomeActivity
        , palette = AnalyticsView.PositivePalette
        , tone = AnalyticsView.IncomeTone
        }
    , negative =
        { label = "Expenses"
        , monthActivityTitle = "Expenses calendar"
        , yearActivityTitle = "Expense activity"
        , activityKind = Analytics.ExpenseActivity
        , palette = AnalyticsView.NegativePalette
        , tone = AnalyticsView.ExpenseTone
        }
    }


analysisReport : Analytics.CashflowResponse -> Analysis.Report
analysisReport response =
    { displayCommodity = response.displayCommodity
    , timeline =
        if response.timelineAvailable then
            Analysis.TimelineAvailable response.timeline

        else
            Analysis.TimelineUnavailable
                ("Cashflow is unavailable because at least one participating amount "
                    ++ "cannot be converted to the display commodity."
                )
                response.timeline
    , positiveBreakdown = response.incomeBreakdown
    , negativeBreakdown = response.expenseBreakdown
    , warnings = response.warnings
    }


performEffects : List Analysis.Effect -> Cmd Msg
performEffects effects =
    effects |> List.map performEffect |> Cmd.batch


performEffect : Analysis.Effect -> Cmd Msg
performEffect effect =
    case effect of
        Analysis.FetchReport token request ->
            Api.getCashflow request (ResponseLoaded token)

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
        Just selectedTransaction ->
            transactionView theme selectedTransaction

        Nothing ->
            dashboard theme viewport data


transactionView : Theme -> Transaction -> Element Msg
transactionView theme selectedTransaction =
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
                        (Button.backButton theme
                            { onPress = AnalysisMsg Analysis.Back }
                        )
                , title = Typography.pageTitle theme "Transaction Detail"
                , subtitle = Nothing
                , actions = []
                }
        , Ui.pageContainer [ TransactionView.transactionDetail theme selectedTransaction ]
        ]


dashboard : Theme -> Viewport -> ModelData -> Element Msg
dashboard theme viewport data =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color theme.pageBg
        , Element.scrollbarY
        , Common.uiClass "page-scroll analytics-page"
        ]
        [ Ui.pageHeaderBand theme <|
            Ui.pageHeader theme
                { leading = Nothing
                , title = Typography.pageTitle theme "Cashflow"
                , subtitle = Just "Income, expenses, categories, and daily activity"
                , actions =
                    [ Element.map AnalysisMsg (Analysis.controlsView theme data.analysis)
                    , Button.iconButton theme
                        { icon = Icon.Refresh, title = "Refresh", onPress = Refresh }
                    ]
                }
        , Ui.pageContainer
            (queryPanel theme viewport data
                :: dashboardContent theme viewport data
            )
        ]


queryPanel : Theme -> Viewport -> ModelData -> Element Msg
queryPanel theme viewport data =
    QueryPanel.view
        { theme = theme
        , compact = Viewport.isCompact (Viewport.contentLayout viewport)
        , draftQuery = data.draftQuery
        , onDraftChanged = DraftChanged
        , onApply = ApplyQuery
        , onClear = ClearQuery
        , queryError = queryError data.response
        , helpText =
            "Filter this entire dashboard with an hledger query. Category, timeline, "
                ++ "and activity results always use the same applied filter."
        , examples =
            [ "acct:expenses"
            , "desc:grocery"
            , "date:2026-01-01..2026-07-31"
            , "cur:INR"
            , "status:*"
            ]
        , baseId = "cashflow-query"
        }


dashboardContent : Theme -> Viewport -> ModelData -> List (Element Msg)
dashboardContent theme viewport data =
    case RemoteData.value data.response of
        Nothing ->
            case data.response of
                Failure error _ ->
                    [ Ui.errorBanner theme (ApiError.message error)
                    , Button.secondaryButton theme { label = "Retry", onPress = Refresh }
                    ]

                _ ->
                    [ cashflowSkeleton theme viewport ]

        Just response ->
            [ States.remoteNotice ApiError.message theme response.metadata.freshness data.response
            , Typography.mutedBody theme
                (String.fromInt response.filteredCount
                    ++ " of "
                    ++ String.fromInt response.totalCount
                    ++ " transactions"
                )
            , Element.map AnalysisMsg
                (Analysis.view
                    theme
                    viewport
                    analysisConfig
                    (analysisReport response)
                    data.analysis
                )
            ]


queryError : RemoteData ApiError value -> Maybe ApiError
queryError remote =
    case remote of
        Failure error _ ->
            if ApiError.isQueryError error then
                Just error

            else
                Nothing

        _ ->
            Nothing


cashflowSkeleton : Theme -> Viewport -> Element Msg
cashflowSkeleton theme viewport =
    let
        layout =
            Viewport.contentLayout viewport

        isCompact =
            Viewport.isCompact layout
    in
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ]
        [ Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
            [ Skeleton.inputBar theme
            , Skeleton.chipRow theme
            ]
        , Skeleton.chartCard theme
        , if isCompact then
            Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ]
                [ Skeleton.chartCard theme
                , Skeleton.chartCard theme
                ]

          else
            Element.row [ Element.width Element.fill, Element.spacing Tokens.space16 ]
                [ Skeleton.chartCard theme
                , Skeleton.chartCard theme
                ]
        , if isCompact then
            Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ]
                [ Skeleton.chartCard theme
                , Skeleton.chartCard theme
                ]

          else
            Element.row [ Element.width Element.fill, Element.spacing Tokens.space16 ]
                [ Skeleton.chartCard theme
                , Skeleton.chartCard theme
                ]
        ]
