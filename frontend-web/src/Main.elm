module Main exposing (main)

import Api.Bootstrap exposing (Bootstrap)
import Api.Client as Api
import Api.Error as ApiError exposing (ApiError)
import Api.Metadata exposing (Freshness(..))
import Browser
import Browser.Events
import Browser.Navigation as Nav
import Element exposing (Element)
import Element.Font as Font
import Html.Attributes
import Json.Decode as Decode
import Page
import Ports
import Preferences exposing (Preferences, ThemeMode(..))
import Pwa
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))
import State.Viewport exposing (Viewport)
import Ui
import Ui.Skeleton
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import Url exposing (Url)
import View.Shell as Shell



-- MODEL


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
    , page : Page.Model
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

        initialRoute =
            Route.fromUrl url

        ( pageModel, pageCmd, _ ) =
            Page.init
                (pageContext
                    initialRoute
                    { width = flags.viewportWidth, height = flags.viewportHeight }
                    (Preferences.theme (Preferences.current initialTheme))
                    initialTheme
                )
                Nothing
    in
    ( { key = key
      , route = initialRoute
      , viewport = { width = flags.viewportWidth, height = flags.viewportHeight }
      , preferences = Preferences.current initialTheme
      , pwa = Pwa.fromFlags flags.isStandalone flags.isIos
      , bootstrap = Loading Nothing
      , page = pageModel
      }
    , Cmd.batch
        [ redirect
        , Api.getBootstrap BootstrapLoaded
        , Cmd.map GotPageMsg pageCmd
        , Ports.registerServiceWorker ()
        ]
    )


pageContext : Route -> Viewport -> Theme -> ThemeMode -> Page.Context
pageContext route viewport theme themeMode =
    { route = route
    , viewport = viewport
    , theme = theme
    , themeMode = themeMode
    }



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | Navigate Route
    | UrlChanged Url
    | Resized Int Int
    | BootstrapLoaded (Result ApiError Bootstrap)
    | RefreshBootstrap
    | ConfigLoaded Decode.Value
    | PwaMsg Pwa.Msg
    | GotPageMsg Page.Msg
    | NoOp


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

                ( pageModel, pageCmd, outMsg ) =
                    Page.init
                        (pageContext route model.viewport (Preferences.theme model.preferences) model.preferences.themeMode)
                        (Just model.page)

                ( finalModel, outCmd ) =
                    applyPageOutMsg outMsg { model | route = route, page = pageModel }
            in
            ( finalModel
            , Cmd.batch [ Cmd.map GotPageMsg pageCmd, outCmd ]
            )

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

        ConfigLoaded value ->
            case Decode.decodeValue Preferences.decoder value of
                Ok preferences ->
                    ( { model | preferences = preferences }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        PwaMsg pwaMsg ->
            let
                ( pwa, command ) =
                    Pwa.update pwaMsg model.pwa
            in
            ( { model | pwa = pwa }, Cmd.map PwaMsg command )

        GotPageMsg pageMsg ->
            let
                ( pageModel, pageCmd, outMsg ) =
                    Page.update
                        (pageContext model.route model.viewport (Preferences.theme model.preferences) model.preferences.themeMode)
                        pageMsg
                        model.page

                ( finalModel, outCmd ) =
                    applyPageOutMsg outMsg { model | page = pageModel }
            in
            ( finalModel
            , Cmd.batch [ Cmd.map GotPageMsg pageCmd, outCmd ]
            )

        NoOp ->
            ( model, Cmd.none )


applyPageOutMsg : Page.OutMsg -> Model -> ( Model, Cmd Msg )
applyPageOutMsg outMsg model =
    case outMsg of
        Page.NoOutMsg ->
            ( model, Cmd.none )

        Page.Navigate route ->
            ( model, Nav.pushUrl model.key (Route.toPath route) )

        Page.SetThemeMode mode ->
            let
                preferences =
                    Preferences.setTheme mode model.preferences
            in
            ( { model | preferences = preferences }, Ports.storeConfig (Preferences.encode preferences) )



-- VIEW


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
        [ Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
            }
            []
          <|
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
    in
    if isRealPage model.route then
        Element.map GotPageMsg
            (Page.view (pageContext model.route model.viewport theme model.preferences.themeMode) model.page)

    else
        Element.column
            [ Element.width Element.fill
            , Element.htmlAttribute (Html.Attributes.class "page-scroll")
            ]
            [ Ui.pageHeaderBand theme device <|
                Ui.pageHeader theme
                    { leading = Nothing
                    , title = Typography.pageTitle theme (pageTitle model.route)
                    , subtitle = Nothing
                    , actions = []
                    }
            , Ui.pageContainer device <|
                bootstrapStatus theme model.bootstrap
                    :: placeholder theme model.route
            ]


isRealPage : Route -> Bool
isRealPage _ =
    True


bootstrapStatus : Theme -> RemoteData Bootstrap -> Element Msg
bootstrapStatus theme remote =
    case remote of
        NotAsked ->
            Ui.loading theme "Preparing application metadata…"

        Loading Nothing ->
            Ui.card theme [ Ui.Skeleton.loading theme ]

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
                ("Loading, success, empty, stale, partial, unavailable, and failed states use shared "
                    ++ "primitives here before endpoint-specific UI is added."
                )
            ]
        , Ui.emptyState theme "No screen data is requested during Phase 3."
        ]
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

        Account accountName ->
            accountName

        AccountValue accountName ->
            accountName ++ " value"

        Transactions ->
            "Transactions"

        Settings ->
            "Settings"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize Resized
        , Ports.configLoaded ConfigLoaded
        , Ports.swStatus (Pwa.SwStatus >> PwaMsg)
        , Ports.promptInstallReady (\_ -> PwaMsg Pwa.PromptInstallReady)
        ]



-- MAIN


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
