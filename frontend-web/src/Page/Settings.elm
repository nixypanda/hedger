module Page.Settings exposing (Model, Msg(..), OutMsg(..), init, update, view)

import Api.Ledgers as Ledgers
import Element exposing (Element)
import Ui.Env exposing (Env)
import Ui.Input
import Ui.LedgerPicker
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
    | LedgerSelected String


type OutMsg
    = SetTheme Mode
    | SelectLedger String


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        ModeSelected mode ->
            ( { model | mode = mode }
            , Cmd.none
            , SetTheme mode
            )

        LedgerSelected ledgerId ->
            ( model, Cmd.none, SelectLedger ledgerId )



-- VIEW


view : Env -> List Ledgers.Ledger -> String -> Model -> Element Msg
view env ledgers selectedLedger model =
    let
        theme =
            env.theme
    in
    Page.scaffold env.viewport
        theme
        { header =
            { leading = Nothing
            , title = Typography.pageTitle theme "Settings"
            , subtitle = Nothing
            , actions = []
            }
        , body =
            [ Surface.formSection theme
                "Ledger"
                [ Ui.LedgerPicker.view theme (Just selectedLedger) ledgers LedgerSelected ]
            , appearanceSection theme model
            ]
        }


appearanceSection : Theme -> Model -> Element Msg
appearanceSection theme model =
    Surface.formSection theme
        "Appearance"
        [ themeToggle theme model ]


themeToggle : Theme -> Model -> Element Msg
themeToggle theme model =
    Ui.Input.segmentedControl theme
        { label = "Theme"
        , selected = model.mode
        , options =
            { first = { label = "Light", value = Light }
            , rest = [ { label = "Dark", value = Dark } ]
            }
        , onSelect = ModeSelected
        }
