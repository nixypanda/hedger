module Main exposing (main)

import Api.Bootstrap exposing (Bootstrap)
import Api.Client as Api
import Api.Error exposing (ApiError)
import Browser
import Browser.Events
import Browser.Navigation as Nav
import Chrome
import Element exposing (Element)
import Json.Decode as Decode
import Page
import Ports
import Preferences exposing (Preferences)
import Pwa
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))
import Ui.Env exposing (Env)
import Ui.Page as UiPage
import Ui.Theme
import Url exposing (Url)



-- MODEL


type alias Flags =
    { isIos : Bool
    , isStandalone : Bool
    , viewportWidth : Int
    , viewportHeight : Int
    , initialMode : String
    }


type alias Model =
    { key : Nav.Key
    , route : Route
    , env : Env
    , preferences : Preferences
    , pwa : Pwa.Model
    , bootstrap : RemoteData ApiError Bootstrap
    , page : Page.Model
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initialMode =
            Ui.Theme.modeFromStringResult flags.initialMode
                |> Result.withDefault Ui.Theme.Light

        initialViewport =
            { width = flags.viewportWidth, height = flags.viewportHeight }

        initialEnv =
            { theme = Ui.Theme.forMode initialMode
            , device = Element.classifyDevice initialViewport
            , viewport = initialViewport
            }

        redirect =
            if url.path == "/" then
                Nav.replaceUrl key (Route.toPath NetWorth)

            else
                Cmd.none

        initialRoute =
            Route.fromUrl url

        ( pageModel, pageCmd, _ ) =
            Page.init
                (pageContext initialRoute initialEnv)
                Nothing
    in
    ( { key = key
      , route = initialRoute
      , env = initialEnv
      , preferences = Preferences.current initialMode
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


pageContext : Route -> Env -> Page.Context
pageContext route env =
    { route = route
    , env = env
    }



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | Navigate Route
    | UrlChanged Url
    | Resized Int Int
    | BootstrapLoaded (Result ApiError Bootstrap)
    | RefreshBootstrap
    | JournalChanged String
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
                        (pageContext route model.env)
                        (Just model.page)

                ( finalModel, outCmd ) =
                    applyPageOutMsg outMsg { model | route = route, page = pageModel }
            in
            ( finalModel
            , Cmd.batch [ Cmd.map GotPageMsg pageCmd, outCmd ]
            )

        Resized width height ->
            ( { model | env = Ui.Env.resize model.env width height }, Cmd.none )

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

        JournalChanged _ ->
            let
                ( pageModel, pageCmd, outMsg ) =
                    Page.init
                        (pageContext model.route model.env)
                        (Just model.page)

                ( finalModel, outCmd ) =
                    applyPageOutMsg outMsg
                        { model
                            | page = pageModel
                            , bootstrap = RemoteData.beginLoading model.bootstrap
                        }
            in
            ( finalModel
            , Cmd.batch
                [ Cmd.map GotPageMsg pageCmd
                , outCmd
                , Api.getBootstrap BootstrapLoaded
                ]
            )

        ConfigLoaded value ->
            case Decode.decodeValue Preferences.decoder value of
                Ok preferences ->
                    ( { model
                        | preferences = preferences
                        , env = Ui.Env.withTheme model.env (Ui.Theme.forMode preferences.mode)
                      }
                    , Cmd.none
                    )

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
                        (pageContext model.route model.env)
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

        Page.SetTheme mode ->
            let
                preferences =
                    Preferences.setTheme mode model.preferences
            in
            ( { model
                | preferences = preferences
                , env = Ui.Env.withTheme model.env (Ui.Theme.forMode mode)
              }
            , Ports.storeConfig (Preferences.encode preferences)
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        env =
            model.env

        page =
            pageView model
    in
    { title = Route.title model.route ++ " · Hedger"
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
            UiPage.layout env.theme <|
                Chrome.view
                    { env = env
                    , route = model.route
                    , pwa = model.pwa
                    , page = page
                    , onNavigate = Navigate
                    , onPwa = PwaMsg
                    }
        ]
    }


pageView : Model -> Element Msg
pageView model =
    Element.map GotPageMsg
        (Page.view (pageContext model.route model.env) model.page)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize Resized
        , Ports.configLoaded ConfigLoaded
        , Ports.journalChanged JournalChanged
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
