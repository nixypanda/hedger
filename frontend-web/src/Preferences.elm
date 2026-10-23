module Preferences exposing
    ( Preferences
    , current
    , decoder
    , encode
    , setTheme
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Ui.Theme exposing (Mode)


type alias Preferences =
    { version : Int
    , mode : Mode
    }


current : Mode -> Preferences
current mode =
    { version = 1, mode = mode }


setTheme : Mode -> Preferences -> Preferences
setTheme mode preferences =
    { preferences | version = 1, mode = mode }


decoder : Decoder Preferences
decoder =
    Decode.map2 Preferences
        (Decode.oneOf [ Decode.field "version" Decode.int, Decode.succeed 1 ])
        (Decode.field "themeMode" Decode.string
            |> Decode.andThen
                (Ui.Theme.modeFromStringResult
                    >> Result.map Decode.succeed
                    >> Result.withDefault (Decode.fail "Unknown theme mode.")
                )
        )
        |> Decode.map (\preferences -> { preferences | version = 1 })


encode : Preferences -> Encode.Value
encode preferences =
    Encode.object
        [ ( "version", Encode.int 1 )
        , ( "themeMode", Encode.string (Ui.Theme.modeToString preferences.mode) )
        ]
