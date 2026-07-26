module Data.Completeness exposing (Completeness(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type Completeness
    = Complete
    | Partial
    | Unavailable


decoder : Decoder Completeness
decoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case value of
                    "complete" ->
                        Decode.succeed Complete

                    "partial" ->
                        Decode.succeed Partial

                    "unavailable" ->
                        Decode.succeed Unavailable

                    _ ->
                        Decode.fail ("Unknown completeness: " ++ value)
            )
