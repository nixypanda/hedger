module Data.Account exposing
    ( AccountNode(..)
    , AccountPath
    , Valuation
    , accountPath
    , accountPathToString
    )

import Data.AccountType exposing (AccountType)
import Data.Completeness exposing (Completeness)
import Data.Date exposing (Date)
import Data.Quantity exposing (Quantity)


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
