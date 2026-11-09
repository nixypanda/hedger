module Main exposing (main)

import Api.Bootstrap exposing (Bootstrap)
import Api.Client as Api
import Api.Error exposing (ApiError)
import Api.Ledgers as Ledgers
import Browser
import Browser.Events
import Browser.Navigation as Nav
import Chrome
import Element exposing (Element)
import Html
import Json.Decode as Decode
import LedgerRouting
import Lib.RemoteData as RemoteData exposing (RemoteData(..))
import Page
import Ports
import Preferences exposing (Preferences)
import Pwa
import Route exposing (Route(..))
import String
import Ui.Css
import Ui.DesignTokens as Tokens
import Ui.Env exposing (Env)
import Ui.LedgerPicker
import Ui.Page as UiPage
import Ui.Theme
import Ui.Typography as Typography
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
    , catalog : RemoteData ApiError Ledgers.Catalog
    , selectedLedger : Maybe String
    , location : Url
    , pageEpoch : Int
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

        ( pageModel, pageCmd, _ ) =
            Page.init
                (pageContext Settings initialEnv "default" [])
                Nothing
    in
    ( { key = key
      , route = NetWorth
      , env = initialEnv
      , preferences = Preferences.current initialMode
      , pwa = Pwa.fromFlags flags.isStandalone flags.isIos
      , bootstrap = Loading Nothing
      , catalog = Loading Nothing
      , selectedLedger = Nothing
      , location = url
      , pageEpoch = 0
      , page = pageModel
      }
    , Cmd.batch
        [ Api.getLedgers CatalogLoaded
        , Cmd.map (GotPageMsg 0) pageCmd
        , Ports.registerServiceWorker ()
        ]
    )


pageContext : Route -> Env -> String -> List Ledgers.Ledger -> Page.Context
pageContext route env ledgerId ledgers =
    { route = route
    , env = env
    , ledgerId = ledgerId
    , ledgers = ledgers
    }



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | Navigate Route
    | UrlChanged Url
    | Resized Int Int
    | CatalogLoaded (Result ApiError Ledgers.Catalog)
    | BootstrapLoaded String (Result ApiError Bootstrap)
    | RefreshBootstrap
    | JournalChanged Decode.Value
    | SelectLedger String
    | ConfigLoaded Decode.Value
    | PwaMsg Pwa.Msg
    | GotPageMsg Int Page.Msg
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
            ( model, navigateTo model route )

        UrlChanged url ->
            openLocation url { model | location = url }

        Resized width height ->
            ( { model | env = Ui.Env.resize model.env width height }, Cmd.none )

        CatalogLoaded result ->
            case result of
                Ok catalog ->
                    openLocation model.location { model | catalog = Success catalog }

                Err error ->
                    ( { model | catalog = Failure error Nothing }, Cmd.none )

        BootstrapLoaded ledgerId result ->
            if model.selectedLedger /= Just ledgerId then
                ( model, Cmd.none )

            else
                case result of
                    Ok bootstrap ->
                        ( { model | bootstrap = Success bootstrap }, Cmd.none )

                    Err error ->
                        ( { model
                            | bootstrap = Failure error (RemoteData.value model.bootstrap)
                          }
                        , Cmd.none
                        )

        RefreshBootstrap ->
            case model.selectedLedger of
                Just ledgerId ->
                    ( { model | bootstrap = RemoteData.beginLoading model.bootstrap }
                    , Api.getBootstrap ledgerId (BootstrapLoaded ledgerId)
                    )

                Nothing ->
                    ( model, Cmd.none )

        JournalChanged value ->
            case Decode.decodeValue (Decode.field "ledgerId" Decode.string) value of
                Ok ledgerId ->
                    if model.selectedLedger == Just ledgerId then
                        refreshLedger ledgerId model

                    else
                        ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        SelectLedger ledgerId ->
            case RemoteData.value model.catalog of
                Just catalog ->
                    if Ledgers.find ledgerId catalog == Nothing then
                        ( model, Cmd.none )

                    else if model.selectedLedger == Just ledgerId then
                        ( model, Cmd.none )

                    else
                        let
                            destination =
                                LedgerRouting.selectionRoute
                                    model.selectedLedger
                                    model.route
                                    model.location
                        in
                        ( model, Nav.pushUrl model.key (LedgerRouting.scopedPath ledgerId destination) )

                Nothing ->
                    ( model, Cmd.none )

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

        GotPageMsg epoch pageMsg ->
            case model.selectedLedger of
                Just ledgerId ->
                    if epoch /= model.pageEpoch then
                        ( model, Cmd.none )

                    else
                        let
                            ( pageModel, pageCmd, outMsg ) =
                                Page.update
                                    (pageContext model.route model.env ledgerId (modelLedgers model))
                                    pageMsg
                                    model.page

                            ( finalModel, outCmd ) =
                                applyPageOutMsg outMsg { model | page = pageModel }
                        in
                        ( finalModel
                        , Cmd.batch [ Cmd.map (GotPageMsg epoch) pageCmd, outCmd ]
                        )

                Nothing ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


applyPageOutMsg : Page.OutMsg -> Model -> ( Model, Cmd Msg )
applyPageOutMsg outMsg model =
    case outMsg of
        Page.NoOutMsg ->
            ( model, Cmd.none )

        Page.Navigate route ->
            ( model, navigateTo model route )

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

        Page.SelectLedger ledgerId ->
            update (SelectLedger ledgerId) model


navigateTo : Model -> Route -> Cmd Msg
navigateTo model route =
    (case model.selectedLedger of
        Just ledgerId ->
            Just ledgerId

        Nothing ->
            RemoteData.value model.catalog |> Maybe.andThen .defaultLedger
    )
        |> Maybe.map (\ledgerId -> Nav.pushUrl model.key (LedgerRouting.scopedPath ledgerId route))
        |> Maybe.withDefault (Nav.pushUrl model.key (Route.toPath route))


modelLedgers : Model -> List Ledgers.Ledger
modelLedgers model =
    model.catalog
        |> RemoteData.value
        |> Maybe.map .ledgers
        |> Maybe.withDefault []


openLocation : Url -> Model -> ( Model, Cmd Msg )
openLocation url model =
    case RemoteData.value model.catalog of
        Nothing ->
            ( model, Cmd.none )

        Just catalog ->
            if url.path == "/" then
                case catalog.defaultLedger of
                    Just ledgerId ->
                        ( model, Nav.replaceUrl model.key (LedgerRouting.scopedPath ledgerId NetWorth) )

                    Nothing ->
                        ( { model | selectedLedger = Nothing, route = NetWorth }
                        , Ports.setActiveLedger ""
                        )

            else
                case LedgerRouting.scopedLocation url of
                    Just ( ledgerId, route ) ->
                        case Ledgers.find ledgerId catalog of
                            Just _ ->
                                let
                                    changed =
                                        model.selectedLedger /= Just ledgerId

                                    epoch =
                                        model.pageEpoch + 1

                                    ( pageModel, pageCmd, outMsg ) =
                                        Page.init
                                            (pageContext route model.env ledgerId catalog.ledgers)
                                            (if changed then
                                                Nothing

                                             else
                                                Just model.page
                                            )

                                    next =
                                        { model
                                            | selectedLedger = Just ledgerId
                                            , route = route
                                            , page = pageModel
                                            , pageEpoch = epoch
                                            , bootstrap = Loading Nothing
                                        }

                                    ( finalModel, outCmd ) =
                                        applyPageOutMsg outMsg next
                                in
                                ( finalModel
                                , Cmd.batch
                                    [ Cmd.map (GotPageMsg epoch) pageCmd
                                    , Api.getBootstrap ledgerId (BootstrapLoaded ledgerId)
                                    , if changed then
                                        Ports.setActiveLedger ledgerId

                                      else
                                        Cmd.none
                                    , outCmd
                                    ]
                                )

                            Nothing ->
                                ( { model
                                    | selectedLedger = Nothing
                                    , pageEpoch = model.pageEpoch + 1
                                  }
                                , Ports.setActiveLedger ""
                                )

                    Nothing ->
                        case LedgerRouting.entryRedirect catalog.defaultLedger url of
                            Just destination ->
                                ( model, Nav.replaceUrl model.key destination )

                            Nothing ->
                                ( { model
                                    | selectedLedger = Nothing
                                    , pageEpoch = model.pageEpoch + 1
                                  }
                                , Ports.setActiveLedger ""
                                )


refreshLedger : String -> Model -> ( Model, Cmd Msg )
refreshLedger ledgerId model =
    let
        epoch =
            model.pageEpoch + 1

        ( pageModel, pageCmd, outMsg ) =
            Page.init
                (pageContext model.route model.env ledgerId (modelLedgers model))
                (Just model.page)

        ( finalModel, outCmd ) =
            applyPageOutMsg outMsg
                { model
                    | page = pageModel
                    , pageEpoch = epoch
                    , bootstrap = RemoteData.beginLoading model.bootstrap
                }
    in
    ( finalModel
    , Cmd.batch
        [ Cmd.map (GotPageMsg epoch) pageCmd
        , outCmd
        , Api.getBootstrap ledgerId (BootstrapLoaded ledgerId)
        ]
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
    { title =
        (if model.selectedLedger == Nothing then
            "Choose a ledger"

         else
            Route.title model.route
        )
            ++ " · Hedger"
    , body =
        [ Html.node "style"
            []
            [ Html.text
                (":root, :root[data-theme] { "
                    ++ Ui.Css.themeCssVariables env.theme
                    ++ " }"
                )
            ]
        , Element.layoutWith
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
    case model.selectedLedger of
        Just ledgerId ->
            Element.map (GotPageMsg model.pageEpoch)
                (Page.view
                    (pageContext model.route model.env ledgerId (modelLedgers model))
                    model.page
                )

        Nothing ->
            case model.catalog of
                Failure error _ ->
                    Element.text ("Could not load ledgers: " ++ error.message)

                Success catalog ->
                    Element.column
                        [ Element.width Element.fill
                        , Element.spacing Tokens.space24
                        , Element.padding Tokens.space24
                        ]
                        [ Typography.pageTitle model.env.theme
                            (if String.startsWith "/ledgers/" model.location.path then
                                "Ledger not found. Choose a ledger."

                             else
                                "Choose a ledger"
                            )
                        , Ui.LedgerPicker.view model.env.theme Nothing catalog.ledgers SelectLedger
                        ]

                _ ->
                    Element.text "Loading ledgers..."



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
