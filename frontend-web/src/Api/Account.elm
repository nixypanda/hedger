module Api.Account exposing
    ( Response
    , decoder
    )

import Api.Metadata as Metadata
import Data.Account exposing (AccountNode(..), Valuation, accountPath)
import Data.AccountType as AccountType exposing (AccountType)
import Data.Completeness as Completeness
import Data.Date as Date
import Data.Metadata exposing (Metadata)
import Data.Quantity as Quantity exposing (Quantity)
import Json.Decode as Decode exposing (Decoder)


type alias Response =
    { metadata : Metadata
    , displayCommodity : String
    , accounts : List AccountNode
    }


type AccountNodeDto
    = AccountNodeDto
        { path : String
        , name : String
        , accountType : Maybe AccountType
        , directBalances : List Quantity
        , subtreeBalances : List Quantity
        , valuation : Valuation
        , children : List AccountNodeDto
        }


decoder : Decoder Response
decoder =
    Decode.map3 Response
        (Decode.field "metadata" Metadata.decoder)
        (Decode.field "displayCommodity" Decode.string)
        (Decode.field "accounts" (Decode.list accountDtoDecoder)
            |> Decode.map (List.map toAccountNode)
        )


accountDtoDecoder : Decoder AccountNodeDto
accountDtoDecoder =
    Decode.map7
        (\path name accountType directBalances subtreeBalances valuation children ->
            AccountNodeDto
                { path = path
                , name = name
                , accountType = accountType
                , directBalances = directBalances
                , subtreeBalances = subtreeBalances
                , valuation = valuation
                , children = children
                }
        )
        (Decode.field "path" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "accountType" (Decode.nullable AccountType.decoder))
        (Decode.field "directBalances" (Decode.list Quantity.decoder))
        (Decode.field "subtreeBalances" (Decode.list Quantity.decoder))
        (Decode.field "valuation" valuationDecoder)
        (Decode.field "children" (Decode.list (Decode.lazy (\_ -> accountDtoDecoder))))


valuationDecoder : Decoder Valuation
valuationDecoder =
    Decode.map5 Valuation
        (Decode.field "asOf" Date.decoder)
        (Decode.field "commodity" Decode.string)
        (Decode.field "directBalances" (Decode.list Quantity.decoder))
        (Decode.field "subtreeBalances" (Decode.list Quantity.decoder))
        (Decode.field "completeness" Completeness.decoder)


toAccountNode : AccountNodeDto -> AccountNode
toAccountNode (AccountNodeDto dto) =
    AccountNode
        { path = accountPath dto.path
        , name = dto.name
        , accountType = dto.accountType
        , directBalances = dto.directBalances
        , subtreeBalances = dto.subtreeBalances
        , valuation = dto.valuation
        , children = List.map toAccountNode dto.children
        }
