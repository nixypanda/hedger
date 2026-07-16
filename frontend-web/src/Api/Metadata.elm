module Api.Metadata exposing (Freshness(..), Metadata, decoder, empty)

import Json.Decode as Decode exposing (Decoder)


type Freshness
    = Fresh
    | Stale
    | Refreshing
    | Partial
    | Unavailable


type alias Metadata =
    { revision : Maybe String
    , freshness : Freshness
    , warnings : List String
    }


empty : Metadata
empty =
    { revision = Nothing, freshness = Fresh, warnings = [] }


decoder : Decoder Metadata
decoder =
    Decode.map3 Metadata
        (optional "revision" Decode.string Nothing Just)
        (optional "freshness" freshnessDecoder Fresh identity)
        (optional "warnings" (Decode.list Decode.string) [] identity)


freshnessDecoder : Decoder Freshness
freshnessDecoder =
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


optional : String -> Decoder a -> b -> (a -> b) -> Decoder b
optional field valueDecoder fallback wrap =
    Decode.oneOf
        [ Decode.field field valueDecoder |> Decode.map wrap
        , Decode.succeed fallback
        ]
