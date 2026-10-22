module Chrome exposing (view)

import Element exposing (Element)
import Pwa
import Route exposing (Route(..))
import Ui.Env exposing (Env)
import Ui.Icon as Icon exposing (Icon)
import View.Chrome


view :
    { env : Env
    , route : Route
    , pwa : Pwa.Model
    , page : Element msg
    , onNavigate : Route -> msg
    , onPwa : Pwa.Msg -> msg
    }
    -> Element msg
view config =
    View.Chrome.view config.env
        { notice = shellNotice config.onPwa config.pwa
        , navigation = shellNavigation config.onNavigate config.route
        , page = config.page
        }


shellNavigation : (Route -> msg) -> Route -> List (View.Chrome.Destination msg)
shellNavigation onNavigate currentRoute =
    [ destination onNavigate currentRoute "Net Worth" Icon.TrendingUp NetWorth
    , destination onNavigate currentRoute "Investments" Icon.ChartBar Investments
    , destination onNavigate currentRoute "Cashflow" Icon.ChartPie Cashflow
    , destination onNavigate currentRoute "Accounts" Icon.Wallet Accounts
    , destination onNavigate currentRoute "Transactions" Icon.Receipt Transactions
    , destination onNavigate currentRoute "Settings" Icon.Settings Settings
    ]


destination :
    (Route -> msg)
    -> Route
    -> String
    -> Icon
    -> Route
    -> View.Chrome.Destination msg
destination onNavigate currentRoute label icon route =
    { label = label
    , icon = icon
    , active = routeIsActive currentRoute route
    , onPress = onNavigate route
    }


routeIsActive : Route -> Route -> Bool
routeIsActive currentRoute destinationRoute =
    case ( currentRoute, destinationRoute ) of
        ( Account _, Accounts ) ->
            True

        ( AccountValue _, Accounts ) ->
            True

        _ ->
            currentRoute == destinationRoute


shellNotice : (Pwa.Msg -> msg) -> Pwa.Model -> Maybe (View.Chrome.Notice msg)
shellNotice onPwa pwa =
    case pwa.serviceWorker of
        Pwa.ServiceWorkerUpdateReady ->
            Just
                { icon = Icon.Refresh
                , message = "A new version of Hedger is ready."
                , actions =
                    [ shellAction onPwa "Update" Pwa.ApplyServiceWorkerUpdate
                    ]
                }

        _ ->
            installationNotice onPwa pwa.installation


installationNotice :
    (Pwa.Msg -> msg)
    -> Pwa.InstallationState
    -> Maybe (View.Chrome.Notice msg)
installationNotice onPwa installation =
    case installation of
        Pwa.IosBrowser Pwa.InstallHintVisible ->
            Just
                { icon = Icon.Info
                , message = "Add Hedger to your Home Screen from Safari's Share menu."
                , actions = [ dismissAction onPwa ]
                }

        Pwa.InstallableBrowser Pwa.InstallPromptAvailable ->
            Just
                { icon = Icon.Download
                , message = "Install Hedger for quick offline access."
                , actions =
                    [ shellAction onPwa "Install" Pwa.PromptInstall
                    , dismissAction onPwa
                    ]
                }

        _ ->
            Nothing


shellAction :
    (Pwa.Msg -> msg)
    -> String
    -> Pwa.Msg
    -> View.Chrome.NoticeAction msg
shellAction onPwa label pwaMsg =
    { label = label
    , onPress = onPwa pwaMsg
    , style = View.Chrome.PrimaryAction
    }


dismissAction : (Pwa.Msg -> msg) -> View.Chrome.NoticeAction msg
dismissAction onPwa =
    { label = "Dismiss"
    , onPress = onPwa Pwa.DismissInstallPrompt
    , style = View.Chrome.GhostAction
    }
