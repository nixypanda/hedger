module Ui.LedgerPicker exposing (view)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes as Attr
import Ui.DesignTokens as Tokens
import Ui.Theme exposing (Theme)


type alias Option =
    { id : String
    , label : String
    , status : String
    }


view : Theme -> Maybe String -> List Option -> (String -> msg) -> Element msg
view theme selected ledgers onSelect =
    Element.column
        [ Element.width Element.fill
        , Element.spacing Tokens.space12
        ]
        (List.map (option theme selected onSelect) ledgers)


option : Theme -> Maybe String -> (String -> msg) -> Option -> Element msg
option theme selected onSelect ledger =
    let
        isSelected =
            selected == Just ledger.id
    in
    Input.button
        [ Element.width Element.fill
        , Element.padding Tokens.space20
        , Border.rounded Tokens.radiusControl
        , Border.width Tokens.borderHairline
        , Border.color theme.navigationActiveFg
        , Background.color
            (if isSelected then
                theme.navigationActiveBg

             else
                theme.cardBg
            )
        , Font.color
            (if isSelected then
                theme.navigationActiveFg

             else
                theme.textPrimary
            )
        , Element.htmlAttribute
            (Attr.attribute "aria-pressed"
                (if isSelected then
                    "true"

                 else
                    "false"
                )
            )
        ]
        { onPress = Just (onSelect ledger.id)
        , label =
            Element.row
                [ Element.width Element.fill ]
                [ Element.el [ Element.width Element.fill ] (Element.text ledger.label)
                , Element.text ledger.status
                ]
        }
