module Page.AccountRegister exposing (view)

import Element exposing (Device, Element)
import Element.Font as Font
import Html.Attributes
import Ui
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography


view : Theme -> Device -> String -> Element msg
view theme device accountName =
    Element.column
        [ Element.width Element.fill
        , Element.htmlAttribute (Html.Attributes.class "page-scroll")
        ]
        [ Ui.pageHeaderBand theme
            device
            (Ui.pageHeader theme
                { leading = Nothing
                , title = Typography.pageTitle theme accountName
                , subtitle = Just "Account register"
                , actions = []
                }
            )
        , Ui.pageContainer device
            [ Ui.card theme
                [ Typography.sectionTitle theme "Register coming soon"
                , Element.paragraph [ Font.color theme.textMedium ]
                    [ Element.text "The account register will be added in Phase 5B." ]
                , Ui.emptyState theme "No register data is loaded yet."
                ]
            ]
        ]
