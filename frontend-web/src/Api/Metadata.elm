module Api.Metadata exposing (Metadata, decoder, empty)

import Data.Freshness as Freshness exposing (Freshness)
import Json.Decode as Decode exposing (Decoder)


type alias Metadata =
    { revision : Maybe String
    , freshness : Freshness
    , warnings : List String
    }


empty : Metadata
empty =
    { revision = Nothing, freshness = Freshness.Fresh, warnings = [] }


decoder : Decoder Metadata
decoder =
    Decode.map3 Metadata
        (optional "revision" Decode.string Nothing Just)
        (optional "freshness" Freshness.decoder Freshness.Fresh identity)
        (optional "warnings" (Decode.list Decode.string) [] identity)


optional : String -> Decoder a -> b -> (a -> b) -> Decoder b
optional field valueDecoder fallback wrap =
    Decode.oneOf
        [ Decode.field field valueDecoder |> Decode.map wrap
        , Decode.succeed fallback
        ]
