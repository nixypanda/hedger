module View.Chrome exposing
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
import Html.Attributes as Attr
import Ui.Button as Button
import Ui.Common as Common
import Ui.DesignTokens as Tokens
import Ui.Env exposing (Env)
import Ui.Icon as Icon exposing (Icon)
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import Ui.Viewport as Viewport


type alias Config msg =
    { notice : Maybe (Notice msg)
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


view : Env -> Config msg -> Element msg
view env config =
    let
        theme =
            env.theme
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color theme.pageBg
        , Common.uiClass "app-shell"
        ]
        [ noticeView theme config.notice
        , Element.row
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.clipY
            , Common.uiClass "shell-body"
            ]
            (if Viewport.usesBottomNavigation env.viewport then
                [ content config.page ]

             else
                [ sidebar env config, content config.page ]
            )
        , if Viewport.usesBottomNavigation env.viewport then
            tabBar env config

          else
            Element.none
        ]


content : Element msg -> Element msg
content page =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.clipX
        , Region.mainContent
        , Common.uiClass "app-content"
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
            Button.primaryButton theme
                { label = action.label, onPress = action.onPress }

        GhostAction ->
            Button.ghostButton theme
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


sidebar : Env -> Config msg -> Element msg
sidebar env config =
    let
        theme =
            env.theme
    in
    Element.column
        [ Element.width (Element.px Tokens.sidebarWidth)
        , Element.height Element.fill
        , Background.gradient
            { angle = pi
            , steps =
                [ theme.navigationGradientStart
                , theme.navigationGradientEnd
                ]
            }
        , Element.padding Tokens.space20
        , Element.spacing Tokens.space12
        ]
        [ brand theme
        , Element.column
            [ Element.width Element.fill
            , Element.spacing Tokens.space12
            , Region.navigation
            , Element.htmlAttribute (Attr.attribute "aria-label" "Primary navigation")
            ]
            (List.map (navItem theme) config.navigation)
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
            , Border.rounded Tokens.radiusControl
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
            [ Typography.fontSize Typography.Lg
            , Typography.fontWeight Typography.Bold
            , Font.color theme.navigationFg
            , Element.centerY
            ]
            (Element.text "Hedger")
        ]


navItem : Theme -> Destination msg -> Element msg
navItem theme destination =
    let
        foreground =
            if destination.active then
                theme.navigationActiveFg

            else
                theme.navigationFg
    in
    Input.button
        [ Element.width Element.fill
        , Common.touchTargetHeight
        , Element.paddingXY Tokens.space16 Tokens.space12
        , Background.color
            (if destination.active then
                theme.navigationActiveBg

             else
                theme.transparent
            )
        , Border.rounded Tokens.radiusControl
        , Font.color foreground
        , Common.uiClass "ui-motion"
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


tabBar : Env -> Config msg -> Element msg
tabBar env config =
    let
        theme =
            env.theme
    in
    Element.row
        [ Element.width Element.fill
        , Element.height (Element.px Tokens.navigationBarHeight)
        , Background.color theme.surface
        , Border.color theme.divider
        , Border.widthEach
            { top = Tokens.borderHairline
            , right = Tokens.space0
            , bottom = Tokens.space0
            , left = Tokens.space0
            }
        , Element.paddingXY Tokens.space0 Tokens.space4
        , Region.navigation
        , Element.htmlAttribute (Attr.attribute "aria-label" "Primary navigation")
        ]
        (List.map (tabItem theme) config.navigation)


tabItem : Theme -> Destination msg -> Element msg
tabItem theme destination =
    let
        foreground =
            if destination.active then
                theme.primary

            else
                theme.textMuted
    in
    Input.button
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color
            (if destination.active then
                theme.selectedBg

             else
                theme.transparent
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
                    (Typography.caption
                        ++ [ Font.center
                           , Element.width Element.fill
                           ]
                    )
                    (Element.text destination.label)
                ]
        }
