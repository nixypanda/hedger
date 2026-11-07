module Pwa exposing
    ( InstallHintState(..)
    , InstallPromptState(..)
    , InstallationState(..)
    , Model
    , Msg(..)
    , NoticeKind(..)
    , NoticeState(..)
    , ServiceWorkerState(..)
    , fromFlags
    , isAnimating
    , update
    )

import Ports
import Time
import Ui.Animation as Animation


type alias Model =
    { serviceWorker : ServiceWorkerState
    , installation : InstallationState
    , updateNoticeDismissed : Bool
    , notice : NoticeState
    , noticeVisibility : Animation.Transition Bool
    , reducedMotion : Bool
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


type NoticeKind
    = ServiceWorkerUpdate
    | IosInstallHint
    | InstallPrompt


type NoticeState
    = NoNotice
    | NoticeVisible NoticeKind
    | NoticeDismissing NoticeKind


type Msg
    = SwStatus String
    | PromptInstallReady
    | PromptInstall
    | DismissInstallPrompt
    | DismissUpdateNotice
    | ApplyServiceWorkerUpdate
    | AnimationTick Time.Posix


fromFlags : { isStandalone : Bool, isIos : Bool, reducedMotion : Bool } -> Model
fromFlags flags =
    let
        installation =
            if flags.isStandalone then
                StandaloneApp

            else if flags.isIos then
                IosBrowser InstallHintVisible

            else
                OtherBrowser

        notice =
            case installation of
                IosBrowser InstallHintVisible ->
                    NoticeVisible IosInstallHint

                _ ->
                    NoNotice
    in
    { serviceWorker = ServiceWorkerUnknown
    , installation = installation
    , updateNoticeDismissed = False
    , notice = notice
    , noticeVisibility = Animation.init True
    , reducedMotion = flags.reducedMotion
    }


isAnimating : Model -> Bool
isAnimating model =
    Animation.isRunning model.noticeVisibility


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SwStatus status ->
            let
                serviceWorker =
                    serviceWorkerStateFromStatus status

                next =
                    { model
                        | serviceWorker = serviceWorker
                        , updateNoticeDismissed =
                            if serviceWorker == ServiceWorkerUpdateReady then
                                False

                            else
                                model.updateNoticeDismissed
                    }
            in
            ( refreshNotice next, Cmd.none )

        PromptInstallReady ->
            ( refreshNotice (installPromptReady model), Cmd.none )

        PromptInstall ->
            case ( model.notice, model.installation ) of
                ( NoticeVisible InstallPrompt, InstallableBrowser InstallPromptAvailable ) ->
                    ( beginDismissal InstallPrompt (dismissInstallPrompt model)
                    , Ports.promptInstall ()
                    )

                _ ->
                    ( model, Cmd.none )

        DismissInstallPrompt ->
            case model.notice of
                NoticeVisible kind ->
                    if isInstallNotice kind then
                        ( beginDismissal kind (dismissInstallPrompt model), Cmd.none )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DismissUpdateNotice ->
            case model.notice of
                NoticeVisible ServiceWorkerUpdate ->
                    ( beginDismissal
                        ServiceWorkerUpdate
                        { model | updateNoticeDismissed = True }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ApplyServiceWorkerUpdate ->
            ( model, Ports.skipWaiting () )

        AnimationTick now ->
            let
                noticeVisibility =
                    Animation.update now model.noticeVisibility

                notice =
                    finishDismissal noticeVisibility model.notice
            in
            ( refreshNotice
                { model | notice = notice, noticeVisibility = noticeVisibility }
            , Cmd.none
            )


refreshNotice : Model -> Model
refreshNotice model =
    case model.notice of
        NoNotice ->
            case desiredNotice model of
                Just kind ->
                    showNotice kind model

                Nothing ->
                    model

        NoticeVisible current ->
            case desiredNotice model of
                Just desired ->
                    if current == desired then
                        model

                    else
                        showNotice desired model

                Nothing ->
                    beginDismissal current model

        NoticeDismissing _ ->
            -- Keep the exiting notice mounted until its timeline arrives.
            -- `AnimationTick` shows any still-desired notice after unmounting.
            model


showNotice : NoticeKind -> Model -> Model
showNotice kind model =
    { model
        | notice = NoticeVisible kind
        , noticeVisibility = Animation.init True
    }


beginDismissal : NoticeKind -> Model -> Model
beginDismissal kind model =
    case model.notice of
        NoticeVisible visibleKind ->
            if visibleKind == kind then
                startDismissal kind model

            else
                model

        _ ->
            model


startDismissal : NoticeKind -> Model -> Model
startDismissal kind model =
    let
        noticeVisibility =
            Animation.to
                { reducedMotion = model.reducedMotion }
                noticeExitDuration
                False
                model.noticeVisibility
    in
    if Animation.isRunning noticeVisibility then
        { model
            | notice = NoticeDismissing kind
            , noticeVisibility = noticeVisibility
        }

    else
        { model | notice = NoNotice, noticeVisibility = noticeVisibility }


finishDismissal : Animation.Transition Bool -> NoticeState -> NoticeState
finishDismissal noticeVisibility notice =
    case notice of
        NoticeDismissing _ ->
            if Animation.isRunning noticeVisibility then
                notice

            else
                NoNotice

        _ ->
            notice


desiredNotice : Model -> Maybe NoticeKind
desiredNotice model =
    if model.serviceWorker == ServiceWorkerUpdateReady && not model.updateNoticeDismissed then
        Just ServiceWorkerUpdate

    else
        case model.installation of
            IosBrowser InstallHintVisible ->
                Just IosInstallHint

            InstallableBrowser InstallPromptAvailable ->
                Just InstallPrompt

            _ ->
                Nothing


isInstallNotice : NoticeKind -> Bool
isInstallNotice kind =
    kind == IosInstallHint || kind == InstallPrompt


noticeExitDuration : Animation.Duration
noticeExitDuration =
    160


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
