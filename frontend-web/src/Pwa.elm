module Pwa exposing
    ( InstallHintState(..)
    , InstallPromptState(..)
    , InstallationState(..)
    , Model
    , Msg(..)
    , ServiceWorkerState(..)
    , fromFlags
    , update
    )

import Ports


type alias Model =
    { serviceWorker : ServiceWorkerState
    , installation : InstallationState
    , updateNoticeDismissed : Bool
    }


type ServiceWorkerState
    = ServiceWorkerUnknown
    | ServiceWorkerInstalling String
    | ServiceWorkerUpdateReady
    | ServiceWorkerControlling


type InstallPromptState
    = InstallPromptAvailable
    | InstallPromptDismissed


type InstallHintState
    = InstallHintVisible
    | InstallHintDismissed


type InstallationState
    = IosBrowser InstallHintState
    | InstallableBrowser InstallPromptState
    | OtherBrowser
    | StandaloneApp


type Msg
    = SwStatus String
    | PromptInstallReady
    | PromptInstall
    | DismissInstallPrompt
    | DismissUpdateNotice
    | ApplyServiceWorkerUpdate


fromFlags : Bool -> Bool -> Model
fromFlags isStandalone isIos =
    { serviceWorker = ServiceWorkerUnknown
    , installation =
        if isStandalone then
            StandaloneApp

        else if isIos then
            IosBrowser InstallHintVisible

        else
            OtherBrowser
    , updateNoticeDismissed = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SwStatus status ->
            let
                serviceWorker =
                    serviceWorkerStateFromStatus status
            in
            ( { model
                | serviceWorker = serviceWorker
                , updateNoticeDismissed =
                    if serviceWorker == ServiceWorkerUpdateReady then
                        False

                    else
                        model.updateNoticeDismissed
              }
            , Cmd.none
            )

        PromptInstallReady ->
            ( installPromptReady model, Cmd.none )

        PromptInstall ->
            case model.installation of
                InstallableBrowser InstallPromptAvailable ->
                    ( dismissInstallPrompt model, Ports.promptInstall () )

                _ ->
                    ( model, Cmd.none )

        DismissInstallPrompt ->
            ( dismissInstallPrompt model, Cmd.none )

        DismissUpdateNotice ->
            ( { model | updateNoticeDismissed = True }, Cmd.none )

        ApplyServiceWorkerUpdate ->
            ( model, Ports.skipWaiting () )


serviceWorkerStateFromStatus : String -> ServiceWorkerState
serviceWorkerStateFromStatus status =
    if status == "update-ready" then
        ServiceWorkerUpdateReady

    else if status == "controllerchange" then
        ServiceWorkerControlling

    else
        ServiceWorkerInstalling status


installPromptReady : Model -> Model
installPromptReady model =
    case model.installation of
        OtherBrowser ->
            { model | installation = InstallableBrowser InstallPromptAvailable }

        InstallableBrowser _ ->
            { model | installation = InstallableBrowser InstallPromptAvailable }

        _ ->
            model


dismissInstallPrompt : Model -> Model
dismissInstallPrompt model =
    case model.installation of
        IosBrowser InstallHintVisible ->
            { model | installation = IosBrowser InstallHintDismissed }

        InstallableBrowser InstallPromptAvailable ->
            { model | installation = InstallableBrowser InstallPromptDismissed }

        _ ->
            model
