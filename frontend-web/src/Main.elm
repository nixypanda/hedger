module Main exposing (main)

import Api.Bootstrap exposing (Bootstrap)
import Api.Client as Api
import Api.Error as ApiError exposing (ApiError)
import Api.Metadata exposing (Freshness(..))
import Api.Transaction
import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Nav
import Element exposing (Element)
import Element.Font as Font
import Html.Attributes
import Json.Decode as Decode
import Page.Transactions as TransactionsPage
import Ports
import Preferences exposing (Preferences, ThemeMode(..))
import Pwa
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))
import State.Viewport exposing (Viewport)
import Task
import Ui
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import Url exposing (Url)
import View.Charts as Charts
import View.Shell as Shell


type alias Flags =
    { isIos : Bool
    , isStandalone : Bool
    , viewportWidth : Int
    , viewportHeight : Int
    , initialThemeMode : String
    }


type alias Model =
    { key : Nav.Key
    , route : Route
    , viewport : Viewport
    , preferences : Preferences
    , pwa : Pwa.Model
    , bootstrap : RemoteData Bootstrap
    , transactions : TransactionsPage.Model
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | Navigate Route
    | UrlChanged Url
    | Resized Int Int
    | BootstrapLoaded (Result ApiError Bootstrap)
    | RefreshBootstrap
    | TransactionsLoaded (Result ApiError Api.Transaction.Response)
    | RefreshTransactions
    | SelectTransaction Api.Transaction.Transaction
    | BackToTransactions
    | ConfigLoaded Decode.Value
    | ToggleTheme
    | PwaMsg Pwa.Msg
    | NoOp


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initialTheme =
            Preferences.themeModeFromString flags.initialThemeMode
                |> Result.withDefault Light

        redirect =
            if url.path == "/" then
                Nav.replaceUrl key (Route.toPath NetWorth)

            else
                Cmd.none
    in
    ( { key = key
      , route = Route.fromUrl url
      , viewport = { width = flags.viewportWidth, height = flags.viewportHeight }
      , preferences = Preferences.current initialTheme
      , pwa = Pwa.fromFlags flags.isStandalone flags.isIos
      , bootstrap = Loading Nothing
      , transactions =
            if Route.fromUrl url == Transactions then
                TransactionsPage.beginLoading TransactionsPage.init

            else
                TransactionsPage.init
      }
    , Cmd.batch
        [ redirect
        , Api.getBootstrap BootstrapLoaded
        , if Route.fromUrl url == Transactions then
            Api.getTransactions TransactionsLoaded

          else
            Cmd.none
        , Ports.registerServiceWorker ()
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked request ->
            case request of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        Navigate route ->
            ( model, Nav.pushUrl model.key (Route.toPath route) )

        UrlChanged url ->
            let
                route =
                    Route.fromUrl url
            in
            if route == Transactions then
                ( { model
                    | route = route
                    , transactions = TransactionsPage.beginLoading model.transactions
                  }
                , Api.getTransactions TransactionsLoaded
                )

            else
                ( { model | route = route }, Cmd.none )

        Resized width height ->
            ( { model | viewport = { width = width, height = height } }, Cmd.none )

        BootstrapLoaded result ->
            case result of
                Ok bootstrap ->
                    ( { model | bootstrap = Success bootstrap }, Cmd.none )

                Err error ->
                    ( { model | bootstrap = Failure error (RemoteData.value model.bootstrap) }, Cmd.none )

        RefreshBootstrap ->
            ( { model | bootstrap = RemoteData.beginLoading model.bootstrap }
            , Api.getBootstrap BootstrapLoaded
            )

        TransactionsLoaded result ->
            ( { model | transactions = TransactionsPage.receive result model.transactions }, Cmd.none )

        RefreshTransactions ->
            ( { model | transactions = TransactionsPage.beginLoading model.transactions }
            , Api.getTransactions TransactionsLoaded
            )

        SelectTransaction transaction ->
            ( { model | transactions = TransactionsPage.select transaction model.transactions }
            , if Ui.isSmall (Element.classifyDevice model.viewport) then
                focus "transaction-detail"

              else
                Cmd.none
            )

        BackToTransactions ->
            ( { model | transactions = TransactionsPage.clearSelection model.transactions }
            , focus "transaction-list"
            )

        ConfigLoaded value ->
            case Decode.decodeValue Preferences.decoder value of
                Ok preferences ->
                    ( { model | preferences = preferences }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ToggleTheme ->
            let
                mode =
                    case model.preferences.themeMode of
                        Light ->
                            Dark

                        Dark ->
                            Light

                preferences =
                    Preferences.setTheme mode model.preferences
            in
            ( { model | preferences = preferences }, Ports.storeConfig (Preferences.encode preferences) )

        PwaMsg pwaMsg ->
            let
                ( pwa, command ) =
                    Pwa.update pwaMsg model.pwa
            in
            ( { model | pwa = pwa }, Cmd.map PwaMsg command )

        NoOp ->
            ( model, Cmd.none )


focus : String -> Cmd Msg
focus elementId =
    Task.attempt (always NoOp) (Dom.focus elementId)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize Resized
        , Ports.configLoaded ConfigLoaded
        , Ports.swStatus (Pwa.SwStatus >> PwaMsg)
        , Ports.promptInstallReady (\_ -> PwaMsg Pwa.PromptInstallReady)
        ]


view : Model -> Browser.Document Msg
view model =
    let
        activeTheme =
            Preferences.theme model.preferences

        page =
            pageView activeTheme model
    in
    { title = pageTitle model.route ++ " · Hedger"
    , body =
        [ Element.layout [] <|
            Ui.layout activeTheme <|
                Shell.view
                    { route = model.route
                    , viewport = model.viewport
                    , theme = activeTheme
                    , pwa = model.pwa
                    , page = page
                    , onNavigate = Navigate
                    , onPwa = PwaMsg
                    }
        ]
    }


pageView : Theme -> Model -> Element Msg
pageView theme model =
    let
        device =
            Element.classifyDevice model.viewport

        title =
            pageTitle model.route

        subtitle =
            if model.route == Review then
                "Reusable visual fixtures for the application foundation."

            else
                "The application shell is ready; this financial screen arrives in a later phase."

        actions =
            if model.route == Settings then
                [ Ui.secondaryButton theme
                    { label =
                        case model.preferences.themeMode of
                            Light ->
                                "Use dark theme"

                            Dark ->
                                "Use light theme"
                    , onPress = ToggleTheme
                    }
                ]

            else
                []
    in
    if model.route == Transactions then
        TransactionsPage.view theme
            model.viewport
            { model = model.transactions
            , onRefresh = RefreshTransactions
            , onSelect = SelectTransaction
            , onBack = BackToTransactions
            }

    else
        Element.column
            [ Element.width Element.fill
            , Element.htmlAttribute (Html.Attributes.class "page-scroll")
            ]
            [ Ui.pageHeaderBand theme device <|
                Ui.pageHeader theme
                    { leading = Nothing
                    , title = Typography.pageTitle theme title
                    , subtitle = Just subtitle
                    , actions = actions
                    }
            , Ui.pageContainer device <|
                bootstrapStatus theme model.bootstrap
                    :: (if model.route == Review then
                            reviewGallery theme

                        else
                            placeholder theme model.route
                       )
            ]


bootstrapStatus : Theme -> RemoteData Bootstrap -> Element Msg
bootstrapStatus theme remote =
    case remote of
        NotAsked ->
            Ui.loading theme "Preparing application metadata…"

        Loading Nothing ->
            Ui.card theme [ Ui.skeletonLoading theme ]

        Loading (Just bootstrap) ->
            Ui.card theme
                [ Ui.statusBadge theme Ui.StatusPrimary "Refreshing metadata"
                , bootstrapSummary theme bootstrap
                ]

        Success bootstrap ->
            case bootstrap.metadata.freshness of
                Fresh ->
                    bootstrapSummary theme bootstrap

                Refreshing ->
                    Ui.card theme
                        [ Ui.statusBadge theme Ui.StatusPrimary "Refreshing"
                        , bootstrapSummary theme bootstrap
                        ]

                Stale ->
                    Ui.card theme
                        [ Ui.statusBadge theme Ui.StatusWarning "Stale snapshot"
                        , bootstrapSummary theme bootstrap
                        ]

                Partial ->
                    Ui.card theme
                        [ Ui.statusBadge theme Ui.StatusWarning "Partial data"
                        , bootstrapSummary theme bootstrap
                        ]

                Unavailable ->
                    Ui.errorBanner theme "Journal metadata is unavailable."

        Failure error stale ->
            let
                failureBanner prefix =
                    Ui.errorBanner theme (prefix ++ ApiError.message error)
            in
            case stale of
                Just bootstrap ->
                    Ui.card theme
                        [ failureBanner "Refresh failed; showing stale metadata: "
                        , bootstrapSummary theme bootstrap
                        ]

                Nothing ->
                    Ui.card theme [ failureBanner "Bootstrap failed: " ]


bootstrapSummary : Theme -> Bootstrap -> Element msg
bootstrapSummary theme bootstrap =
    Element.wrappedRow [ Element.width Element.fill, Element.spacing 12 ]
        [ Ui.statusBadge theme Ui.StatusSuccess (bootstrap.appName ++ " API " ++ bootstrap.apiVersion)
        , Ui.badge theme ("Fiscal year starts in month " ++ String.fromInt bootstrap.fiscalYearStartMonth)
        , bootstrap.metadata.revision
            |> Maybe.map (\revision -> Ui.badge theme ("Revision " ++ revision))
            |> Maybe.withDefault Element.none
        ]


placeholder : Theme -> Route -> List (Element Msg)
placeholder theme route =
    [ Ui.card theme
        [ Typography.sectionTitle theme (pageTitle route ++ " placeholder")
        , Element.paragraph [ Font.color theme.textMedium ]
            [ Element.text
                "Loading, success, empty, stale, partial, unavailable, and failed states use shared primitives here before endpoint-specific UI is added."
            ]
        , Ui.emptyState theme "No screen data is requested during Phase 3."
        ]
    ]


reviewGallery : Theme -> List (Element Msg)
reviewGallery theme =
    [ Ui.card theme
        [ Typography.sectionTitle theme "Controls and states"
        , Element.wrappedRow [ Element.spacing 8 ]
            [ Ui.primaryButton theme { label = "Primary", onPress = RefreshBootstrap }
            , Ui.secondaryButton theme { label = "Secondary", onPress = RefreshBootstrap }
            , Ui.statusBadge theme Ui.StatusSuccess "Fresh"
            , Ui.statusBadge theme Ui.StatusWarning "Stale"
            , Ui.statusBadge theme Ui.StatusDanger "Unavailable"
            ]
        ]
    , Ui.card theme
        [ Typography.sectionTitle theme "Prepared-series chart primitive"
        , Charts.barChart theme
            [ { label = "Apr", value = 42 }
            , { label = "May", value = 68 }
            , { label = "Jun", value = 51 }
            , { label = "Jul", value = 84 }
            ]
        ]
    , Ui.card theme
        [ TransactionsPage.reviewFixtures theme NoOp ]
    ]


pageTitle : Route -> String
pageTitle route =
    case route of
        NetWorth ->
            "Net Worth"

        Investments ->
            "Investments"

        Cashflow ->
            "Cashflow"

        Accounts ->
            "Accounts"

        Transactions ->
            "Transactions"

        Settings ->
            "Settings"

        Review ->
            "Review Gallery"
