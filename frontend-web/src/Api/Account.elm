module Api.Account exposing
    ( AccountNode(..)
    , AccountPath
    , Response
    , Valuation
    , accountPath
    , accountPathToString
    , decoder
    )

import Api.Metadata as Metadata exposing (Metadata)
import Data.AccountType as AccountType exposing (AccountType)
import Data.Completeness as Completeness exposing (Completeness)
import Data.Date as Date exposing (Date)
import Data.Quantity as Quantity exposing (Quantity)
import Json.Decode as Decode exposing (Decoder)


type AccountPath
    = AccountPath String


accountPath : String -> AccountPath
accountPath value =
    AccountPath value


accountPathToString : AccountPath -> String
accountPathToString (AccountPath value) =
    value


type AccountNode
    = AccountNode
        { path : AccountPath
        , name : String
        , accountType : Maybe AccountType
        , directBalances : List Quantity
        , subtreeBalances : List Quantity
        , valuation : Valuation
        , children : List AccountNode
        }


type alias Valuation =
    { asOf : Date
    , commodity : String
    , directBalances : List Quantity
    , subtreeBalances : List Quantity
    , completeness : Completeness
    }


type alias Response =
    { metadata : Metadata
    , displayCommodity : String
    , accounts : List AccountNode
    }


decoder : Decoder Response
decoder =
    Decode.map3 Response
        (Decode.field "metadata" Metadata.decoder)
        (Decode.field "displayCommodity" Decode.string)
        (Decode.field "accounts" (Decode.list accountDecoder))


accountDecoder : Decoder AccountNode
accountDecoder =
    Decode.map7
        (\path name accountType directBalances subtreeBalances valuation children ->
            AccountNode
                { path = accountPath path
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
        (Decode.field "children" (Decode.list (Decode.lazy (\_ -> accountDecoder))))


valuationDecoder : Decoder Valuation
valuationDecoder =
    Decode.map5 Valuation
        (Decode.field "asOf" Date.decoder)
        (Decode.field "commodity" Decode.string)
        (Decode.field "directBalances" (Decode.list Quantity.decoder))
        (Decode.field "subtreeBalances" (Decode.list Quantity.decoder))
        (Decode.field "completeness" Completeness.decoder)
