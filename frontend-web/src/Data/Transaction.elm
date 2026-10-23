module Data.Transaction exposing
    ( Amount
    , Cost
    , Posting
    , Presentation
    , Tag
    , Transaction
    , TransactionEffect(..)
    , effectDecoder
    )

import Data.AccountType exposing (AccountType)
import Data.Date exposing (Date)
import Data.Decimal exposing (Decimal)
import Data.Status exposing (Status)
import Json.Decode as Decode exposing (Decoder)


type alias Transaction =
    { primaryDate : Date
    , secondaryDate : Maybe Date
    , status : Status
    , code : Maybe String
    , description : String
    , comment : Maybe String
    , tags : List Tag
    , postings : List Posting
    , presentation : Presentation
    }


type alias Posting =
    { account : String
    , accountType : Maybe AccountType
    , status : Status
    , comment : Maybe String
    , tags : List Tag
    , amounts : List Amount
    }


type TransactionEffect
    = TransactionIncome
    | TransactionExpense
    | TransactionTransfer
    | OpeningBalance
    | Borrowing
    | Repayment
    | AssetAcquisition
    | AssetDisposal
    | AssetReallocation
    | AssetTransfer
    | Refinancing
    | DebtForgiveness
    | BorrowingReversal
    | IncomeReversal
    | ExpenseReversal
    | Unclassified
    | Unknown String


type alias Presentation =
    { classification : TransactionEffect
    , effects : List TransactionEffect
    }


effectDecoder : Decoder TransactionEffect
effectDecoder =
    Decode.string
        |> Decode.map
            (\label ->
                case label of
                    "income" ->
                        TransactionIncome

                    "expense" ->
                        TransactionExpense

                    "transfer" ->
                        TransactionTransfer

                    "opening_balance" ->
                        OpeningBalance

                    "borrowing" ->
                        Borrowing

                    "repayment" ->
                        Repayment

                    "asset_acquisition" ->
                        AssetAcquisition

                    "asset_disposal" ->
                        AssetDisposal

                    "asset_reallocation" ->
                        AssetReallocation

                    "asset_transfer" ->
                        AssetTransfer

                    "refinancing" ->
                        Refinancing

                    "debt_forgiveness" ->
                        DebtForgiveness

                    "borrowing_reversal" ->
                        BorrowingReversal

                    "income_reversal" ->
                        IncomeReversal

                    "expense_reversal" ->
                        ExpenseReversal

                    "unclassified" ->
                        Unclassified

                    _ ->
                        Unknown label
            )


type alias Amount =
    { commodity : String
    , quantity : Decimal
    , cost : Maybe Cost
    }


type alias Cost =
    { costType : String
    , commodity : String
    , quantity : Decimal
    }


type alias Tag =
    { name : String
    , value : String
    }
