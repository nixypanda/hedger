module Preferences exposing
    ( Preferences
    , ThemeMode(..)
    , current
    , decoder
    , encode
    , setTheme
    , theme
    , themeModeFromString
    , themeModeToString
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Ui.Theme


type ThemeMode
    = Light
    | Dark


type alias Preferences =
    { version : Int
    , themeMode : ThemeMode
    }


current : ThemeMode -> Preferences
current mode =
    { version = 1, themeMode = mode }


setTheme : ThemeMode -> Preferences -> Preferences
setTheme mode preferences =
    { preferences | version = 1, themeMode = mode }


theme : Preferences -> Ui.Theme.Theme
theme preferences =
    case preferences.themeMode of
        Light ->
            Ui.Theme.forMode Ui.Theme.Light

        Dark ->
            Ui.Theme.forMode Ui.Theme.Dark


decoder : Decoder Preferences
decoder =
    Decode.map2 Preferences
        (Decode.oneOf [ Decode.field "version" Decode.int, Decode.succeed 1 ])
        (Decode.field "themeMode" Decode.string
            |> Decode.andThen
                (themeModeFromString
                    >> Result.map Decode.succeed
                    >> Result.withDefault (Decode.fail "Unknown theme mode.")
                )
        )
        |> Decode.map (\preferences -> { preferences | version = 1 })


encode : Preferences -> Encode.Value
encode preferences =
    Encode.object
        [ ( "version", Encode.int 1 )
        , ( "themeMode", Encode.string (themeModeToString preferences.themeMode) )
        ]


themeModeFromString : String -> Result String ThemeMode
themeModeFromString value =
    case value of
        "light" ->
            Ok Light

        "dark" ->
            Ok Dark

        _ ->
            Err "Unknown theme mode."


themeModeToString : ThemeMode -> String
themeModeToString mode =
    case mode of
        Light ->
            "light"

        Dark ->
            "dark"
