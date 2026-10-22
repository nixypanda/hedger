module Page.Settings exposing (Model, Msg(..), OutMsg(..), init, update, view)

import Element exposing (Element)
import Ui.Common as Common
import Ui.Env exposing (Env)
import Ui.Input
import Ui.Page as Page
import Ui.Surface as Surface
import Ui.Theme exposing (Mode(..), Theme)
import Ui.Typography as Typography



-- MODEL


type alias Model =
    { mode : Mode }


init : Mode -> Model
init mode =
    { mode = mode }



-- UPDATE


type Msg
    = ModeSelected Mode


type OutMsg
    = SetTheme Mode


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        ModeSelected mode ->
            ( { model | mode = mode }
            , Cmd.none
            , SetTheme mode
            )



-- VIEW


view : Env -> Model -> Element Msg
view env model =
    let
        theme =
            env.theme
    in
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
            , selected = model.mode == Light
            , onPress = ModeSelected Light
            }
        , Ui.Input.segmentedOption theme
            { label = "Dark"
            , selected = model.mode == Dark
            , onPress = ModeSelected Dark
            }
        ]
