module Api.Ledgers exposing (Catalog, Ledger, decoder, find)

import Json.Decode as Decode exposing (Decoder)


type alias Catalog =
    { defaultLedger : Maybe String
    , ledgers : List Ledger
    }


type alias Ledger =
    { id : String
    , label : String
    , status : String
    }


decoder : Decoder Catalog
decoder =
    Decode.map2 Catalog
        (Decode.field "defaultLedger" (Decode.nullable Decode.string))
        (Decode.field "ledgers" (Decode.list ledgerDecoder))


ledgerDecoder : Decoder Ledger
ledgerDecoder =
    Decode.map3 Ledger
        (Decode.field "id" Decode.string)
        (Decode.field "label" Decode.string)
        (Decode.field "status" Decode.string)


find : String -> Catalog -> Maybe Ledger
find selected catalog =
    catalog.ledgers
        |> List.filter (\ledger -> ledger.id == selected)
        |> List.head
