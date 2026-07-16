module Api.Bootstrap exposing (Bootstrap, decoder)

import Api.Metadata as Metadata exposing (Metadata)
import Json.Decode as Decode exposing (Decoder)


type alias Bootstrap =
    { apiVersion : String
    , appName : String
    , fiscalYearStartMonth : Int
    , defaultValuationBasis : String
    , metadata : Metadata
    }


decoder : Decoder Bootstrap
decoder =
    Decode.map5 Bootstrap
        (Decode.field "apiVersion" Decode.string)
        (Decode.field "appName" Decode.string)
        (Decode.field "fiscalYearStartMonth" Decode.int)
        (Decode.field "defaultValuationBasis" Decode.string)
        (Decode.oneOf
            [ Decode.field "metadata" Metadata.decoder
            , Decode.succeed Metadata.empty
            ]
        )
