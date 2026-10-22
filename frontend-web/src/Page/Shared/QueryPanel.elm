module Page.Shared.QueryPanel exposing (Config, view)

import Api.Error as ApiError exposing (ApiError)
import Element exposing (Element)
import Html.Attributes
import Ui
import Ui.DesignTokens as Tokens
import Ui.Surface as Surface
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography


type alias Config msg =
    { theme : Theme
    , compact : Bool
    , draftQuery : String
    , onDraftChanged : String -> msg
    , onApply : msg
    , onClear : msg
    , queryError : Maybe ApiError
    , helpText : String
    , examples : List String
    , baseId : String
    }


view : Config msg -> Element msg
view config =
    let
        searchInput =
            Ui.searchInputInline config.theme
                { label = "hledger query"
                , text = config.draftQuery
                , onChange = config.onDraftChanged
                , extraAttrs =
                    [ Element.htmlAttribute (Html.Attributes.id config.baseId)
                    , Element.htmlAttribute (Html.Attributes.attribute "autocomplete" "off")
                    , Element.htmlAttribute
                        (Html.Attributes.attribute "aria-describedby"
                            (config.baseId ++ "-help " ++ config.baseId ++ "-error")
                        )
                    ]
                }

        buttons =
            Element.row [ Element.spacing Tokens.space8 ]
                [ Ui.dangerButton config.theme { label = "Clear", onPress = config.onClear }
                , Ui.primaryButton config.theme { label = "Apply", onPress = config.onApply }
                ]

        inputRow =
            if config.compact then
                Element.column
                    [ Element.width Element.fill
                    , Element.spacing Tokens.space8
                    ]
                    [ searchInput
                    , Element.el [ Element.alignRight ] buttons
                    ]

            else
                Element.row
                    [ Element.width Element.fill
                    , Element.spacing Tokens.space8
                    , Element.centerY
                    ]
                    [ searchInput, buttons ]
    in
    Surface.card config.theme
        [ Ui.fieldLabel config.theme "hledger query"
        , inputRow
        , case config.queryError of
            Just error ->
                Element.el
                    [ Element.width Element.fill
                    , Element.htmlAttribute (Html.Attributes.id (config.baseId ++ "-error"))
                    , Element.htmlAttribute (Html.Attributes.attribute "role" "alert")
                    ]
                    (Ui.errorBanner config.theme (ApiError.message error))

            Nothing ->
                Element.none
        , Element.el [ Element.htmlAttribute (Html.Attributes.id (config.baseId ++ "-help")) ]
            (Typography.mutedParagraph config.theme config.helpText)
        , Element.wrappedRow [ Element.width Element.fill, Element.spacing Tokens.space8 ]
            (Typography.mutedCaption config.theme "Examples:"
                :: List.map
                    (\example ->
                        Ui.chip config.theme
                            { label = example
                            , selected = config.draftQuery == example
                            , onClick = Just (config.onDraftChanged example)
                            }
                    )
                    config.examples
            )
        ]
