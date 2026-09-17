module View.Shell exposing
    ( ActionStyle(..)
    , Destination
    , Notice
    , NoticeAction
    , view
    )

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import State.Viewport exposing (Viewport)
import Ui
import Ui.DesignTokens as Tokens
import Ui.Icon as Icon exposing (Icon)
import Ui.Responsive as Responsive
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography


type alias Config msg =
    { viewport : Viewport
    , theme : Theme
    , notice : Maybe (Notice msg)
    , navigation : List (Destination msg)
    , page : Element msg
    }


type alias Notice msg =
    { icon : Icon
    , message : String
    , actions : List (NoticeAction msg)
    }


type alias NoticeAction msg =
    { label : String
    , onPress : msg
    , style : ActionStyle
    }


type ActionStyle
    = PrimaryAction
    | GhostAction


type alias Destination msg =
    { label : String
    , icon : Icon
    , active : Bool
    , onPress : msg
    }


view : Config msg -> Element msg
view config =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color config.theme.pageBg
        , Typography.uiClass "app-shell"
        ]
        [ noticeView config.theme config.notice
        , Element.row
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.clipY
            , Typography.uiClass "shell-body"
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
        , Typography.uiClass "app-content"
        ]
        page


noticeView : Theme -> Maybe (Notice msg) -> Element msg
noticeView theme maybeNotice =
    case maybeNotice of
        Just notice ->
            banner theme
                notice.icon
                notice.message
                (List.map (noticeAction theme) notice.actions)

        Nothing ->
            Element.none


noticeAction : Theme -> NoticeAction msg -> Element msg
noticeAction theme action =
    case action.style of
        PrimaryAction ->
            Ui.primaryButton theme
                { label = action.label, onPress = action.onPress }

        GhostAction ->
            Ui.ghostButton theme
                { label = action.label, onPress = action.onPress }


banner : Theme -> Icon -> String -> List (Element msg) -> Element msg
banner theme icon message actions =
    Element.row
        [ Element.width Element.fill
        , Element.paddingXY Tokens.space16 Tokens.space12
        , Element.spacing Tokens.space12
        , Background.color theme.primarySoft
        , Border.color theme.divider
        , Border.widthEach
            { top = Tokens.space0
            , right = Tokens.space0
            , bottom = Tokens.borderHairline
            , left = Tokens.space0
            }
        ]
        ([ Icon.view { icon = icon, size = Tokens.iconSizeSmall, color = theme.primary }
         , Element.paragraph [ Element.width Element.fill ] [ Element.text message ]
         ]
            ++ actions
        )


sidebar : Config msg -> Element msg
sidebar config =
    Element.column
        [ Element.width (Element.px Tokens.sidebarWidth)
        , Element.height Element.fill
        , Background.gradient
            { angle = pi
            , steps =
                [ config.theme.navigationGradientStart
                , config.theme.navigationGradientEnd
                ]
            }
        , Element.padding Tokens.space20
        , Element.spacing Tokens.space12
        ]
        [ brand config.theme
        , Element.column [ Element.width Element.fill, Element.spacing Tokens.space12 ]
            (List.map (navItem config) config.navigation)
        ]


brand : Theme -> Element msg
brand theme =
    Element.row
        [ Element.spacing Tokens.space12
        , Element.paddingEach
            { top = Tokens.space0
            , right = Tokens.space4
            , bottom = Tokens.space24
            , left = Tokens.space4
            }
        ]
        [ Element.el
            [ Element.width (Element.px Tokens.navigationBrandSize)
            , Element.height (Element.px Tokens.navigationBrandSize)
            , Border.rounded Tokens.radiusMedium
            , Background.color theme.navigationBrandBg
            ]
            (Element.el [ Element.centerX, Element.centerY ]
                (Icon.view
                    { icon = Icon.Wallet
                    , size = Tokens.iconSizeNavigation
                    , color = theme.navigationFg
                    }
                )
            )
        , Element.el
            [ Typography.brandLabel
            , Font.semiBold
            , Font.color theme.navigationFg
            , Element.centerY
            ]
            (Element.text "Hedger")
        ]


navItem : Config msg -> Destination msg -> Element msg
navItem config destination =
    let
        foreground =
            if destination.active then
                config.theme.navigationActiveFg

            else
                config.theme.navigationFg
    in
    Input.button
        [ Element.width Element.fill
        , Ui.touchTargetHeight
        , Element.paddingXY Tokens.space16 Tokens.space12
        , Background.color
            (if destination.active then
                config.theme.navigationActiveBg

             else
                config.theme.transparent
            )
        , Border.rounded Tokens.radiusMedium
        , Font.color foreground
        , Typography.uiClass "ui-motion"
        ]
        { onPress = Just destination.onPress
        , label =
            Element.row [ Element.spacing Tokens.space12 ]
                [ Icon.view
                    { icon = destination.icon
                    , size = Tokens.iconSizeCaption
                    , color = foreground
                    }
                , Element.text destination.label
                ]
        }


tabBar : Config msg -> Element msg
tabBar config =
    Element.row
        [ Element.width Element.fill
        , Element.height (Element.px Tokens.navigationBarHeight)
        , Background.color config.theme.surface
        , Border.color config.theme.divider
        , Border.widthEach
            { top = Tokens.borderHairline
            , right = Tokens.space0
            , bottom = Tokens.space0
            , left = Tokens.space0
            }
        , Element.paddingXY Tokens.space0 Tokens.space4
        ]
        (List.map (tabItem config) config.navigation)


tabItem : Config msg -> Destination msg -> Element msg
tabItem config destination =
    let
        foreground =
            if destination.active then
                config.theme.primary

            else
                config.theme.textMuted
    in
    Input.button
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color
            (if destination.active then
                config.theme.selectedBg

             else
                config.theme.transparent
            )
        , Border.rounded Tokens.radiusControl
        , Font.color foreground
        , Region.description destination.label
        ]
        { onPress = Just destination.onPress
        , label =
            Element.column
                [ Element.centerX
                , Element.centerY
                , Element.spacing Tokens.space4
                ]
                [ Icon.view
                    { icon = destination.icon
                    , size = Tokens.iconSizeCaption
                    , color = foreground
                    }
                , Element.el
                    [ Typography.tabLabel
                    , Font.center
                    , Element.width Element.fill
                    ]
                    (Element.text destination.label)
                ]
        }
