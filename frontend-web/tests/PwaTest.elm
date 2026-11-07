module PwaTest exposing (suite)

import Expect
import Pwa
import Test exposing (Test, describe, test)
import Time


suite : Test
suite =
    describe "PWA notice state machine"
        [ test "iOS browsers start with the install hint" <|
            \_ -> initialIosNotice
        , test "service-worker updates take priority over the install hint" <|
            \_ -> updatePriority
        , test "a dismissed notice stays mounted until the exit timeline arrives" <|
            \_ -> dismissalLifecycle
        , test "repeated dismiss requests during the exit are no-ops" <|
            \_ -> repeatedDismissal
        , test "reduced motion unmounts the notice immediately" <|
            \_ -> reducedMotionDismissal
        , test "a new update-ready event re-shows a dismissed update notice" <|
            \_ -> updateNoticeReturnsOnNewUpdate
        , test "dismissing the install hint does not suppress a later update" <|
            \_ -> installDismissalLeavesUpdates
        , test "dismissing an update does not suppress the install prompt" <|
            \_ -> updateDismissalLeavesInstallPrompt
        , test "the update notice stays mounted through its exit timeline" <|
            \_ -> updateDismissalLifecycle
        , test "the install prompt stays mounted through its exit timeline" <|
            \_ -> installPromptDismissalLifecycle
        , test "installing dismisses the install prompt through the exit timeline" <|
            \_ -> promptInstallLifecycle
        , test "wrong-kind dismiss requests are no-ops" <|
            \_ -> wrongKindDismissals
        , test "a new update-ready during an update exit is shown after arrival" <|
            \_ -> updateDuringDismissal
        ]


iosModel : Bool -> Pwa.Model
iosModel reducedMotion =
    Pwa.fromFlags
        { isStandalone = False
        , isIos = True
        , reducedMotion = reducedMotion
        }


otherModel : Bool -> Pwa.Model
otherModel reducedMotion =
    Pwa.fromFlags
        { isStandalone = False
        , isIos = False
        , reducedMotion = reducedMotion
        }


update : Pwa.Msg -> Pwa.Model -> Pwa.Model
update msg model =
    Pwa.update msg model |> Tuple.first


tick : Int -> Pwa.Model -> Pwa.Model
tick milliseconds model =
    update (Pwa.AnimationTick (Time.millisToPosix milliseconds)) model


initialIosNotice : Expect.Expectation
initialIosNotice =
    iosModel False
        |> .notice
        |> Expect.equal (Pwa.NoticeVisible Pwa.IosInstallHint)


updatePriority : Expect.Expectation
updatePriority =
    iosModel False
        |> update (Pwa.SwStatus "update-ready")
        |> .notice
        |> Expect.equal (Pwa.NoticeVisible Pwa.ServiceWorkerUpdate)


dismissalLifecycle : Expect.Expectation
dismissalLifecycle =
    let
        dismissing =
            iosModel False
                |> update Pwa.DismissInstallPrompt

        started =
            dismissing |> tick 1000

        finished =
            started |> tick 2000
    in
    { dismissingNotice = dismissing.notice
    , dismissingAnimating = Pwa.isAnimating dismissing
    , startedNotice = started.notice
    , startedAnimating = Pwa.isAnimating started
    , finishedNotice = finished.notice
    , finishedAnimating = Pwa.isAnimating finished
    }
        |> Expect.equal
            { dismissingNotice = Pwa.NoticeDismissing Pwa.IosInstallHint
            , dismissingAnimating = True
            , startedNotice = Pwa.NoticeDismissing Pwa.IosInstallHint
            , startedAnimating = True
            , finishedNotice = Pwa.NoNotice
            , finishedAnimating = False
            }


repeatedDismissal : Expect.Expectation
repeatedDismissal =
    let
        dismissing =
            iosModel False
                |> update Pwa.DismissInstallPrompt

        repeated =
            dismissing |> update Pwa.DismissInstallPrompt
    in
    repeated |> Expect.equal dismissing


reducedMotionDismissal : Expect.Expectation
reducedMotionDismissal =
    let
        model =
            iosModel True
                |> update Pwa.DismissInstallPrompt
    in
    ( model.notice, Pwa.isAnimating model )
        |> Expect.equal ( Pwa.NoNotice, False )


updateNoticeReturnsOnNewUpdate : Expect.Expectation
updateNoticeReturnsOnNewUpdate =
    let
        dismissed =
            otherModel False
                |> update (Pwa.SwStatus "update-ready")
                |> update Pwa.DismissUpdateNotice
                |> tick 1000
                |> tick 2000

        refreshed =
            dismissed |> update (Pwa.SwStatus "update-ready")
    in
    ( dismissed.notice, refreshed.notice )
        |> Expect.equal
            ( Pwa.NoNotice, Pwa.NoticeVisible Pwa.ServiceWorkerUpdate )


installDismissalLeavesUpdates : Expect.Expectation
installDismissalLeavesUpdates =
    let
        dismissed =
            otherModel False
                |> update Pwa.PromptInstallReady
                |> update Pwa.DismissInstallPrompt
                |> tick 1000
                |> tick 2000

        updated =
            dismissed |> update (Pwa.SwStatus "update-ready")
    in
    ( dismissed.notice, updated.notice )
        |> Expect.equal
            ( Pwa.NoNotice, Pwa.NoticeVisible Pwa.ServiceWorkerUpdate )


updateDismissalLeavesInstallPrompt : Expect.Expectation
updateDismissalLeavesInstallPrompt =
    let
        dismissed =
            otherModel False
                |> update (Pwa.SwStatus "update-ready")
                |> update Pwa.DismissUpdateNotice
                |> tick 1000
                |> tick 2000

        install =
            dismissed |> update Pwa.PromptInstallReady
    in
    ( dismissed.notice, install.notice )
        |> Expect.equal
            ( Pwa.NoNotice, Pwa.NoticeVisible Pwa.InstallPrompt )


updateDismissalLifecycle : Expect.Expectation
updateDismissalLifecycle =
    lifecycle
        (otherModel False
            |> update (Pwa.SwStatus "update-ready")
            |> update Pwa.DismissUpdateNotice
        )
        Pwa.ServiceWorkerUpdate


installPromptDismissalLifecycle : Expect.Expectation
installPromptDismissalLifecycle =
    lifecycle
        (otherModel False
            |> update Pwa.PromptInstallReady
            |> update Pwa.DismissInstallPrompt
        )
        Pwa.InstallPrompt


promptInstallLifecycle : Expect.Expectation
promptInstallLifecycle =
    lifecycle
        (otherModel False
            |> update Pwa.PromptInstallReady
            |> update Pwa.PromptInstall
        )
        Pwa.InstallPrompt


lifecycle : Pwa.Model -> Pwa.NoticeKind -> Expect.Expectation
lifecycle dismissing kind =
    let
        started =
            dismissing |> tick 1000

        finished =
            started |> tick 2000
    in
    { dismissingNotice = dismissing.notice
    , dismissingAnimating = Pwa.isAnimating dismissing
    , startedNotice = started.notice
    , startedAnimating = Pwa.isAnimating started
    , finishedNotice = finished.notice
    , finishedAnimating = Pwa.isAnimating finished
    }
        |> Expect.equal
            { dismissingNotice = Pwa.NoticeDismissing kind
            , dismissingAnimating = True
            , startedNotice = Pwa.NoticeDismissing kind
            , startedAnimating = True
            , finishedNotice = Pwa.NoNotice
            , finishedAnimating = False
            }


wrongKindDismissals : Expect.Expectation
wrongKindDismissals =
    let
        updateVisible =
            otherModel False |> update (Pwa.SwStatus "update-ready")

        installVisible =
            otherModel False |> update Pwa.PromptInstallReady
    in
    { updateDismissedByInstall =
        updateVisible
            |> update Pwa.DismissInstallPrompt
            |> .notice
    , installDismissedByLater =
        installVisible
            |> update Pwa.DismissUpdateNotice
            |> .notice
    }
        |> Expect.equal
            { updateDismissedByInstall = Pwa.NoticeVisible Pwa.ServiceWorkerUpdate
            , installDismissedByLater = Pwa.NoticeVisible Pwa.InstallPrompt
            }


updateDuringDismissal : Expect.Expectation
updateDuringDismissal =
    let
        dismissing =
            otherModel False
                |> update (Pwa.SwStatus "update-ready")
                |> update Pwa.DismissUpdateNotice

        stillDismissing =
            dismissing |> update (Pwa.SwStatus "update-ready")

        finished =
            stillDismissing |> tick 1000 |> tick 2000
    in
    { during = stillDismissing.notice
    , after = finished.notice
    }
        |> Expect.equal
            { during = Pwa.NoticeDismissing Pwa.ServiceWorkerUpdate
            , after = Pwa.NoticeVisible Pwa.ServiceWorkerUpdate
            }
