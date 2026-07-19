module Page.Cashflow exposing (Model, Msg(..), init, update, view)

import Api.Analytics as Analytics
import Api.Client as Api
import Api.Error as ApiError exposing (ApiError)
import Api.Transaction exposing (Transaction)
import Element exposing (Device, Element)
import Element.Background as Background
import Html.Attributes
import Page.Shared.Analysis as Analysis
import Page.Transactions as Transactions
import RemoteData exposing (RemoteData(..))
import State.Viewport exposing (Viewport)
import Ui
import Ui.Icon as Icon
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
    , analysis : Analysis.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        data =
            { response = Loading Nothing
            , draftQuery = ""
            , appliedQuery = ""
            , analysis = Analysis.init
            }
    in
    ( Model data, fetchReport data.appliedQuery data.analysis )



-- UPDATE


type Msg
    = Refresh
    | DraftChanged String
    | ApplyQuery
    | ClearQuery
    | ResponseLoaded (Result ApiError Analytics.CashflowResponse)
    | AnalysisMsg Analysis.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model data) =
    case msg of
        Refresh ->
            ( Model { data | response = RemoteData.beginLoading data.response }
            , fetchReport data.appliedQuery data.analysis
            )

        DraftChanged draft ->
            ( Model { data | draftQuery = draft }, Cmd.none )

        ApplyQuery ->
            applyQuery (String.trim data.draftQuery) data

        ClearQuery ->
            applyQuery "" { data | draftQuery = "" }

        ResponseLoaded result ->
            case result of
                Err error ->
                    ( Model { data | response = Failure error (RemoteData.value data.response) }, Cmd.none )

                Ok response ->
                    let
                        context =
                            analysisContext data.appliedQuery

                        ( analysis, effects ) =
                            Analysis.reportLoaded context (analysisReport response) data.analysis
                    in
                    ( Model { data | response = Success response, analysis = analysis }
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


analysisReport : Analytics.CashflowResponse -> Analytics.Report
analysisReport response =
    { displayCommodity = response.displayCommodity
    , timeline =
        if response.timelineAvailable then
            Analytics.TimelineAvailable response.timeline

        else
            Analytics.TimelineUnavailable
                ("Cashflow is unavailable because at least one participating amount "
                    ++ "cannot be converted to the display commodity."
                )
                response.timeline
    , positiveBreakdown = response.incomeBreakdown
    , negativeBreakdown = response.expenseBreakdown
    , warnings = response.warnings
    }


fetchReport : String -> Analysis.Model -> Cmd Msg
fetchReport query analysis =
    Api.getCashflow (Analysis.reportRequest query analysis) ResponseLoaded


performEffects : List Analysis.Effect -> Cmd Msg
performEffects effects =
    effects |> List.map performEffect |> Cmd.batch


performEffect : Analysis.Effect -> Cmd Msg
performEffect effect =
    case effect of
        Analysis.FetchReport request ->
            Api.getCashflow request ResponseLoaded

        Analysis.FetchActivity side request ->
            Api.getActivity request (Analysis.ActivityLoaded side >> AnalysisMsg)


isReportEffect : Analysis.Effect -> Bool
isReportEffect effect =
    case effect of
        Analysis.FetchReport _ ->
            True

        Analysis.FetchActivity _ _ ->
            False



-- VIEW


view : Theme -> Viewport -> Model -> Element Msg
view theme viewport (Model data) =
    let
        device =
            Element.classifyDevice viewport
    in
    case Analysis.transaction data.analysis of
        Just selectedTransaction ->
            transactionView theme device selectedTransaction

        Nothing ->
            dashboard theme viewport device data


transactionView : Theme -> Device -> Transaction -> Element Msg
transactionView theme device selectedTransaction =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color theme.pageBg
        , Element.scrollbarY
        ]
        [ Ui.pageHeaderBand theme device <|
            Ui.pageHeader theme
                { leading =
                    Just
                        (Ui.ghostButton theme
                            { label = "← Back", onPress = AnalysisMsg Analysis.Back }
                        )
                , title = Typography.pageTitle theme "Transaction Detail"
                , subtitle = Nothing
                , actions = []
                }
        , Ui.pageContainer device [ Transactions.transactionDetail theme selectedTransaction ]
        ]


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
                    [ Element.map AnalysisMsg (Analysis.controlsView theme data.analysis)
                    , Ui.iconButton device
                        theme
                        { icon = Icon.Refresh, title = "Refresh", onPress = Refresh }
                    ]
                }
        , Ui.pageContainer device
            (queryPanel theme data
                :: dashboardContent theme viewport data
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


dashboardContent : Theme -> Viewport -> ModelData -> List (Element Msg)
dashboardContent theme viewport data =
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
            , Element.map AnalysisMsg
                (Analysis.view theme viewport analysisConfig (analysisReport response) data.analysis)
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


queryError : RemoteData value -> Maybe ApiError
queryError remote =
    case remote of
        Failure error _ ->
            if ApiError.isQueryError error then
                Just error

            else
                Nothing

        _ ->
            Nothing
