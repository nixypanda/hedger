module Api.Json exposing (maybeString, required)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


required : String -> Decoder field -> Decoder (field -> value) -> Decoder value
required name fieldDecoder partialDecoder =
    Decode.map2 (\partial field -> partial field)
        partialDecoder
        (Decode.field name fieldDecoder)


maybeString : Maybe String -> Encode.Value
maybeString =
    Maybe.map Encode.string >> Maybe.withDefault Encode.null
