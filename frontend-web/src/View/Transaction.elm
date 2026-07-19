module View.Transaction exposing (classificationLabel, classificationTone)

import Api.Transaction exposing (Transaction)
import Ui


classificationTone : String -> Ui.FinancialTone
classificationTone classification =
    case classification of
        "income" ->
            Ui.FinancialIncome

        "expense" ->
            Ui.FinancialExpense

        "opening_balance" ->
            Ui.FinancialEquity

        "borrowing" ->
            Ui.FinancialBorrowing

        "repayment" ->
            Ui.FinancialRepayment

        "asset_acquisition" ->
            Ui.FinancialAsset

        "asset_disposal" ->
            Ui.FinancialAsset

        "asset_reallocation" ->
            Ui.FinancialAsset

        _ ->
            Ui.FinancialNeutral


classificationLabel : Transaction -> String
classificationLabel transaction =
    case transaction.presentation.classification of
        "income" ->
            "Income"

        "expense" ->
            "Expense"

        "opening_balance" ->
            "Opening balance"

        "borrowing" ->
            "Borrowing"

        "repayment" ->
            "Repayment"

        "asset_acquisition" ->
            "Asset acquisition"

        "asset_disposal" ->
            "Asset disposal"

        "asset_reallocation" ->
            "Asset reallocation"

        "unclassified" ->
            "Unclassified"

        _ ->
            "Transfer"
