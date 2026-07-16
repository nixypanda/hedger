module View.Shell exposing (view)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Pwa exposing (InstallHintState(..), InstallPromptState(..), InstallationState(..), ServiceWorkerState(..))
import Route exposing (Route(..))
import State.Viewport exposing (Viewport)
import Ui
import Ui.Icon as Icon exposing (Icon)
import Ui.Responsive as Responsive
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography


type alias Config msg =
    { route : Route
    , viewport : Viewport
    , theme : Theme
    , pwa : Pwa.Model
    , page : Element msg
    , onNavigate : Route -> msg
    , onPwa : Pwa.Msg -> msg
    }


view : Config msg -> Element msg
view config =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color config.theme.pageBg
        , Element.htmlAttribute (Html.Attributes.class "app-shell")
        ]
        [ pwaBanner config
        , Element.row
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.clipY
            , Element.htmlAttribute (Html.Attributes.class "shell-body")
            ]
            (if Responsive.usesBottomNavigation config.viewport then
                [ content config.page ]

             else
                [ sidebar config, content config.page ]
            )
        , if Responsive.usesBottomNavigation config.viewport then
            tabBar config

          else
            Element.none
        ]


content : Element msg -> Element msg
content page =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.clipX
        , Element.htmlAttribute (Html.Attributes.class "app-content")
        ]
        page


pwaBanner : Config msg -> Element msg
pwaBanner config =
    case config.pwa.serviceWorker of
        ServiceWorkerUpdateReady ->
            banner config.theme
                Icon.Refresh
                "A new version of Hedger is ready."
                [ Ui.primaryButton config.theme
                    { label = "Update", onPress = config.onPwa Pwa.ApplyServiceWorkerUpdate }
                ]

        _ ->
            case config.pwa.installation of
                IosBrowser InstallHintVisible ->
                    banner config.theme
                        Icon.Info
                        "Add Hedger to your Home Screen from Safari's Share menu."
                        [ dismissButton config ]

                InstallableBrowser InstallPromptAvailable ->
                    banner config.theme
                        Icon.Download
                        "Install Hedger for quick offline access."
                        [ Ui.primaryButton config.theme
                            { label = "Install", onPress = config.onPwa Pwa.PromptInstall }
                        , dismissButton config
                        ]

                _ ->
                    Element.none


banner : Theme -> Icon -> String -> List (Element msg) -> Element msg
banner theme icon message actions =
    Element.row
        [ Element.width Element.fill
        , Element.paddingXY 16 12
        , Element.spacing 12
        , Background.color theme.primarySoft
        , Border.color theme.divider
        , Border.widthEach { top = 0, right = 0, bottom = 1, left = 0 }
        ]
        ([ Icon.view { icon = icon, size = 20, color = theme.primary }
         , Element.paragraph [ Element.width Element.fill ] [ Element.text message ]
         ]
            ++ actions
        )


dismissButton : Config msg -> Element msg
dismissButton config =
    Ui.ghostButton config.theme
        { label = "Dismiss", onPress = config.onPwa Pwa.DismissInstallPrompt }


sidebar : Config msg -> Element msg
sidebar config =
    Element.column
        [ Element.width (Element.px 232)
        , Element.height Element.fill
        , Background.gradient
            { angle = pi
            , steps = [ Element.rgb255 126 48 232, Element.rgb255 69 80 229 ]
            }
        , Element.padding 20
        , Element.spacing 12
        ]
        [ brand
        , Element.column [ Element.width Element.fill, Element.spacing 12 ]
            (List.map (navItem config) navigation)
        ]


brand : Element msg
brand =
    Element.row [ Element.spacing 12, Element.paddingEach { top = 0, right = 4, bottom = 24, left = 4 } ]
        [ Element.el
            [ Element.width (Element.px 34)
            , Element.height (Element.px 34)
            , Border.rounded 10
            , Background.color (Element.rgba255 255 255 255 0.18)
            ]
            (Element.el [ Element.centerX, Element.centerY ]
                (Icon.view { icon = Icon.Wallet, size = 19, color = Element.rgb255 255 255 255 })
            )
        , Element.el
            [ Typography.brandLabel
            , Font.semiBold
            , Font.color (Element.rgb255 255 255 255)
            , Element.centerY
            ]
            (Element.text "Hedger")
        ]


type alias Destination =
    { label : String, icon : Icon, route : Route }


navigation : List Destination
navigation =
    [ { label = "Net Worth", icon = Icon.TrendingUp, route = NetWorth }
    , { label = "Investments", icon = Icon.ChartBar, route = Investments }
    , { label = "Cashflow", icon = Icon.ChartPie, route = Cashflow }
    , { label = "Accounts", icon = Icon.Wallet, route = Accounts }
    , { label = "Transactions", icon = Icon.Receipt, route = Transactions }
    , { label = "Settings", icon = Icon.Settings, route = Settings }
    ]


navItem : Config msg -> Destination -> Element msg
navItem config destination =
    let
        active =
            config.route == destination.route

        foreground =
            if active then
                Element.rgb255 89 49 190

            else
                Element.rgb255 239 236 255
    in
    Input.button
        [ Element.width Element.fill
        , Element.paddingXY 16 12
        , Background.color
            (if active then
                Element.rgb255 255 255 255

             else
                Element.rgba 0 0 0 0
            )
        , Border.rounded 10
        , Font.color foreground
        , Element.htmlAttribute (Html.Attributes.class "ui-motion")
        ]
        { onPress = Just (config.onNavigate destination.route)
        , label =
            Element.row [ Element.spacing 12 ]
                [ Icon.view { icon = destination.icon, size = 18, color = foreground }
                , Element.text destination.label
                ]
        }


tabBar : Config msg -> Element msg
tabBar config =
    Element.row
        [ Element.width Element.fill
        , Element.height (Element.px 64)
        , Background.color config.theme.surface
        , Border.color config.theme.divider
        , Border.widthEach { top = 1, right = 0, bottom = 0, left = 0 }
        , Element.paddingXY 0 4
        ]
        (List.map (tabItem config) navigation)


tabItem : Config msg -> Destination -> Element msg
tabItem config destination =
    let
        active =
            config.route == destination.route

        foreground =
            if active then
                config.theme.primary

            else
                config.theme.textMuted
    in
    Input.button
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color
            (if active then
                config.theme.selectedBg

             else
                Element.rgba 0 0 0 0
            )
        , Border.rounded 6
        , Font.color foreground
        ]
        { onPress = Just (config.onNavigate destination.route)
        , label =
            Element.column [ Element.width Element.fill, Element.spacing 2, Element.centerY ]
                [ Icon.view { icon = destination.icon, size = 18, color = foreground }
                    |> Element.el [ Element.centerX ]
                , Element.el [ Element.centerX, Typography.tabLabel ]
                    (Element.text destination.label)
                ]
        }
