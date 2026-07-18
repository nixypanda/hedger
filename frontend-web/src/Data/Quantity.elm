module Data.Quantity exposing (Quantity, commodity, decoder, quantity)

import Data.Decimal as Decimal exposing (Decimal)
import Json.Decode as Decode exposing (Decoder)


type alias Quantity =
    { commodity : String
    , quantity : Decimal
    }


commodity : Quantity -> String
commodity q =
    q.commodity


quantity : Quantity -> Decimal
quantity q =
    q.quantity


decoder : Decode.Decoder Quantity
decoder =
    Decode.map2 Quantity
        (Decode.field "commodity" Decode.string)
        (Decode.field "quantity" Decimal.decoder)
