module Page.Settings exposing (Model, Msg(..), OutMsg(..), init, update, view)

import Element exposing (Element)
import Html.Attributes
import Preferences exposing (ThemeMode(..))
import Ui
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography



-- MODEL


type alias Model =
    { themeMode : ThemeMode }


init : ThemeMode -> Model
init themeMode =
    { themeMode = themeMode }



-- UPDATE


type Msg
    = ThemeModeSelected ThemeMode


type OutMsg
    = SetThemeMode ThemeMode


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        ThemeModeSelected mode ->
            ( { model | themeMode = mode }
            , Cmd.none
            , SetThemeMode mode
            )



-- VIEW


view : Theme -> Model -> Element Msg
view theme model =
    Element.column
        [ Element.width Element.fill
        , Element.htmlAttribute (Html.Attributes.class "page-scroll")
        ]
        [ Ui.pageHeaderBand theme <|
            Ui.pageHeader theme
                { leading = Nothing
                , title = Typography.pageTitle theme "Settings"
                , subtitle = Nothing
                , actions = []
                }
        , Ui.pageContainer
            [ appearanceSection theme model
            ]
        ]


appearanceSection : Theme -> Model -> Element Msg
appearanceSection theme model =
    Ui.formSection theme
        "Appearance"
        [ themeToggle theme model ]


themeToggle : Theme -> Model -> Element Msg
themeToggle theme model =
    Ui.segmentedControl theme
        [ Ui.segmentedOption theme
            { label = "Light"
            , selected = model.themeMode == Light
            , onPress = ThemeModeSelected Light
            }
        , Ui.segmentedOption theme
            { label = "Dark"
            , selected = model.themeMode == Dark
            , onPress = ThemeModeSelected Dark
            }
        ]
