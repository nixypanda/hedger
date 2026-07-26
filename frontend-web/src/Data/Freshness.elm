module Data.Freshness exposing (Freshness(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type Freshness
    = Fresh
    | Stale
    | Refreshing
    | Partial
    | Unavailable


decoder : Decoder Freshness
decoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case value of
                    "fresh" ->
                        Decode.succeed Fresh

                    "stale" ->
                        Decode.succeed Stale

                    "refreshing" ->
                        Decode.succeed Refreshing

                    "partial" ->
                        Decode.succeed Partial

                    "unavailable" ->
                        Decode.succeed Unavailable

                    _ ->
                        Decode.fail ("Unknown freshness: " ++ value)
            )
