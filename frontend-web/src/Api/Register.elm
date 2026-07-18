module Api.Register exposing (Completeness(..), Entry, Response, Value, decoder)

import Api.Metadata as Metadata exposing (Metadata)
import Data.Date as Date exposing (Date)
import Data.Quantity as Quantity exposing (Quantity)
import Json.Decode as Decode exposing (Decoder)


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
    { date : Date
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
        (Decode.field "date" Date.decoder)
        (Decode.field "description" Decode.string)
        (Decode.field "account" Decode.string)
        (Decode.field "amount" valueDecoder)
        (Decode.field "balance" valueDecoder)


valueDecoder : Decoder Value
valueDecoder =
    Decode.map3 Value
        (Decode.field "raw" (Decode.list Quantity.decoder))
        (Decode.field "display" (Decode.list Quantity.decoder))
        (Decode.field "completeness" completenessDecoder)


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
