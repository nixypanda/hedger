module Data.AccountType exposing (AccountType(..), decoder, toString)

import Json.Decode as Decode exposing (Decoder)


type AccountType
    = Asset
    | Liability
    | Equity
    | Revenue
    | Expense
    | Cash
    | Conversion
    | Gain


toString : AccountType -> String
toString accountType =
    case accountType of
        Asset ->
            "asset"

        Liability ->
            "liability"

        Equity ->
            "equity"

        Revenue ->
            "income"

        Expense ->
            "expense"

        Cash ->
            "cash"

        Conversion ->
            "conversion"

        Gain ->
            "gain"


decoder : Decoder AccountType
decoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case value of
                    "asset" ->
                        Decode.succeed Asset

                    "liability" ->
                        Decode.succeed Liability

                    "equity" ->
                        Decode.succeed Equity

                    "income" ->
                        Decode.succeed Revenue

                    "expense" ->
                        Decode.succeed Expense

                    "cash" ->
                        Decode.succeed Cash

                    "conversion" ->
                        Decode.succeed Conversion

                    "gain" ->
                        Decode.succeed Gain

                    _ ->
                        Decode.fail ("Unknown account type: " ++ value)
            )
