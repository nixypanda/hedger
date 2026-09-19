module Data.Quantity exposing
    ( Quantity
    , commodity
    , commodityCount
    , decoder
    , hasMultipleCommodities
    , quantity
    )

import Data.Decimal as Decimal exposing (Decimal)
import Json.Decode as Decode
import Set


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


hasMultipleCommodities : List Quantity -> Bool
hasMultipleCommodities balances =
    case balances of
        first :: rest ->
            List.any (\balance -> commodity balance /= commodity first) rest

        [] ->
            False


commodityCount : List Quantity -> Int
commodityCount balances =
    balances
        |> List.map commodity
        |> Set.fromList
        |> Set.size


decoder : Decode.Decoder Quantity
decoder =
    Decode.map2 Quantity
        (Decode.field "commodity" Decode.string)
        (Decode.field "quantity" Decimal.decoder)
