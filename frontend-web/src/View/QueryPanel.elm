module View.QueryPanel exposing (Config, view)

import Element exposing (Element)
import Html.Attributes
import Ui.Button as Button
import Ui.Chip as Chip
import Ui.DesignTokens as Tokens
import Ui.Icon as Icon
import Ui.Input
import Ui.Notice as Notice
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
    , queryError : Maybe String
    , helpText : String
    , examples : List String
    , baseId : String
    }


view : Config msg -> Element msg
view config =
    let
        searchInput =
            Ui.Input.searchInputInline config.theme
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
                [ queryActionButton config.theme
                    { tone = Button.Danger
                    , label = "Clear"
                    , onPress = config.onClear
                    }
                , queryActionButton config.theme
                    { tone = Button.Primary
                    , label = "Apply"
                    , onPress = config.onApply
                    }
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
    Element.column [ Element.width Element.fill ]
        [ Surface.card config.theme
            [ Ui.Input.fieldLabel config.theme "hledger query"
            , inputRow
            , Element.el [ Element.htmlAttribute (Html.Attributes.id (config.baseId ++ "-help")) ]
                (Typography.mutedParagraph config.theme config.helpText)
            , Element.wrappedRow [ Element.width Element.fill, Element.spacing Tokens.space8 ]
                (Typography.mutedCaption config.theme "Examples:"
                    :: List.map
                        (\example ->
                            Chip.chip config.theme
                                { label = example
                                , selected = config.draftQuery == example
                                , onClick = Just (config.onDraftChanged example)
                                }
                        )
                        config.examples
                )
            ]
        , queryErrorNotice config
        ]


queryActionButton :
    Theme
    -> { tone : Button.ButtonTone, label : String, onPress : msg }
    -> Element msg
queryActionButton theme config =
    Button.textButton theme
        { tone = config.tone
        , width = Button.ContentWidth
        , label = config.label
        , onPress = config.onPress
        , submitState = Nothing
        , submittingLabel = Nothing
        , trailing = Nothing
        }


queryErrorNotice : Config msg -> Element msg
queryErrorNotice config =
    case config.queryError of
        Just errorMessage ->
            Notice.view config.theme
                { placement = Notice.Bottom
                , tone = Notice.Danger
                , icon = Icon.AlertTriangle
                , message = errorMessage
                , actions = []
                , id = Just (config.baseId ++ "-error")
                }

        Nothing ->
            Element.none
