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
import Html.Attributes
import State.Viewport exposing (Viewport)
import Ui
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
        , Element.htmlAttribute (Html.Attributes.class "app-shell")
        ]
        [ noticeView config.theme config.notice
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
            (List.map (navItem config) config.navigation)
        ]


brand : Element msg
brand =
    Element.row
        [ Element.spacing 12
        , Element.paddingEach { top = 0, right = 4, bottom = 24, left = 4 }
        ]
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


navItem : Config msg -> Destination msg -> Element msg
navItem config destination =
    let
        foreground =
            if destination.active then
                Element.rgb255 89 49 190

            else
                Element.rgb255 239 236 255
    in
    Input.button
        [ Element.width Element.fill
        , Element.paddingXY 16 12
        , Background.color
            (if destination.active then
                Element.rgb255 255 255 255

             else
                Element.rgba 0 0 0 0
            )
        , Border.rounded 10
        , Font.color foreground
        , Element.htmlAttribute (Html.Attributes.class "ui-motion")
        ]
        { onPress = Just destination.onPress
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
        , Element.height (Element.px 72)
        , Background.color config.theme.surface
        , Border.color config.theme.divider
        , Border.widthEach { top = 1, right = 0, bottom = 0, left = 0 }
        , Element.paddingXY 0 4
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
                Element.rgba 0 0 0 0
            )
        , Border.rounded 6
        , Font.color foreground
        , Region.description destination.label
        ]
        { onPress = Just destination.onPress
        , label =
            Element.column
                [ Element.centerX
                , Element.centerY
                , Element.spacing 4
                ]
                [ Icon.view { icon = destination.icon, size = 18, color = foreground }
                , Element.el
                    [ Typography.tabLabel
                    , Font.center
                    , Element.width Element.fill
                    ]
                    (Element.text destination.label)
                ]
        }
