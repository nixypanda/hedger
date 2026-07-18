module Api.Register exposing (Completeness(..), Entry, Quantity, Response, Value, decoder)

import Api.Metadata as Metadata exposing (Metadata)
import Json.Decode as Decode exposing (Decoder)


type alias Quantity =
    { commodity : String
    , quantity : String
    }


type Completeness
    = Complete
    | Partial
    | Unavailable


type alias Value =
    { raw : List Quantity
    , display : List Quantity
    , completeness : Completeness
    }


type alias Entry =
    { date : String
    , description : String
    , account : String
    , amount : Value
    , balance : Value
    }


type alias Response =
    { metadata : Metadata
    , displayCommodity : String
    , entries : List Entry
    , nextCursor : Maybe String
    }


decoder : Decoder Response
decoder =
    Decode.map4 Response
        (Decode.field "metadata" Metadata.decoder)
        (Decode.field "displayCommodity" Decode.string)
        (Decode.field "entries" (Decode.list entryDecoder))
        (Decode.field "nextCursor" (Decode.nullable Decode.string))


entryDecoder : Decoder Entry
entryDecoder =
    Decode.map5 Entry
        (Decode.field "date" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "account" Decode.string)
        (Decode.field "amount" valueDecoder)
        (Decode.field "balance" valueDecoder)


valueDecoder : Decoder Value
valueDecoder =
    Decode.map3 Value
        (Decode.field "raw" (Decode.list quantityDecoder))
        (Decode.field "display" (Decode.list quantityDecoder))
        (Decode.field "completeness" completenessDecoder)


quantityDecoder : Decoder Quantity
quantityDecoder =
    Decode.map2 Quantity
        (Decode.field "commodity" Decode.string)
        (Decode.field "quantity" Decode.string)


completenessDecoder : Decoder Completeness
completenessDecoder =
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
