module Page.Settings exposing (Model, Msg(..), OutMsg(..), init, update, view)

import Element exposing (Element)
import Preferences exposing (ThemeMode(..))
import Ui.Common as Common
import Ui.Input
import Ui.Page as Page
import Ui.Surface as Surface
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
        , Common.uiClass "page-scroll"
        ]
        [ Page.pageHeaderBand theme <|
            Page.pageHeader theme
                { leading = Nothing
                , title = Typography.pageTitle theme "Settings"
                , subtitle = Nothing
                , actions = []
                }
        , Page.pageContainer
            [ appearanceSection theme model
            ]
        ]


appearanceSection : Theme -> Model -> Element Msg
appearanceSection theme model =
    Surface.formSection theme
        "Appearance"
        [ themeToggle theme model ]


themeToggle : Theme -> Model -> Element Msg
themeToggle theme model =
    Ui.Input.segmentedControl theme
        [ Ui.Input.segmentedOption theme
            { label = "Light"
            , selected = model.themeMode == Light
            , onPress = ThemeModeSelected Light
            }
        , Ui.Input.segmentedOption theme
            { label = "Dark"
            , selected = model.themeMode == Dark
            , onPress = ThemeModeSelected Dark
            }
        ]
