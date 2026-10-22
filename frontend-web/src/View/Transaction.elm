module View.Transaction exposing
    ( amountsText
    , classificationLabel
    , classificationTone
    , effectLabel
    , transactionDetail
    )

import Data.Date as Date
import Data.Status as Status
import Data.Transaction exposing (Amount, Posting, Tag, Transaction)
import Element exposing (Element)
import Element.Font as Font
import Ui
import Ui.Common as Common
import Ui.DesignTokens as Tokens
import Ui.Icon as Icon
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import View.AccountPresentation as AccountPresentation
import View.Formatting as Formatting


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

        "asset_transfer" ->
            Ui.FinancialAsset

        "refinancing" ->
            Ui.FinancialBorrowing

        "debt_forgiveness" ->
            Ui.FinancialRepayment

        "borrowing_reversal" ->
            Ui.FinancialRepayment

        "income_reversal" ->
            Ui.FinancialIncome

        "expense_reversal" ->
            Ui.FinancialExpense

        _ ->
            Ui.FinancialNeutral


classificationLabel : Transaction -> String
classificationLabel transaction =
    effectLabel transaction.presentation.classification


effectLabel : String -> String
effectLabel classification =
    case classification of
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

        "asset_transfer" ->
            "Asset transfer"

        "refinancing" ->
            "Refinancing"

        "debt_forgiveness" ->
            "Debt forgiven"

        "borrowing_reversal" ->
            "Borrowing reversed"

        "income_reversal" ->
            "Income reversed"

        "expense_reversal" ->
            "Expense reversed"

        "unclassified" ->
            "Unclassified"

        _ ->
            "Transfer"


transactionDetail : Theme -> Transaction -> Element msg
transactionDetail theme transaction =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ]
        [ Ui.card theme
            (List.concat
                [ [ detailRow theme "Date" (Date.toString transaction.primaryDate) ]
                , maybeDetailRow
                    theme
                    "Secondary date"
                    (Maybe.map Date.toString transaction.secondaryDate)
                , [ detailRow theme "Status" (Status.toString transaction.status) ]
                , maybeDetailRow theme "Code" transaction.code
                , [ detailRow theme "Description" transaction.description ]
                , [ detailRow theme "Effects" (effectSummary transaction) ]
                , maybeDetailRow theme "Comment" transaction.comment
                , if List.isEmpty transaction.tags then
                    []

                  else
                    [ detailRow theme "Tags" (tagsText transaction.tags) ]
                ]
            )
        , Ui.section theme
            { header = Just (Typography.sectionTitle theme "Postings")
            , body =
                if List.isEmpty transaction.postings then
                    [ Typography.mutedParagraph theme "No postings" ]

                else
                    List.intersperse (Ui.hairline theme)
                        (List.map (postingDetailRow theme) transaction.postings)
            , footer = Nothing
            }
        ]


effectSummary : Transaction -> String
effectSummary transaction =
    case transaction.presentation.effects of
        [] ->
            classificationLabel transaction

        effects ->
            effects |> List.map effectLabel |> String.join ", "


detailRow : Theme -> String -> String -> Element msg
detailRow theme label value =
    Element.row
        [ Element.width Element.fill
        , Element.spacing Tokens.space16
        , Element.alignTop
        , Common.minWidthZero
        ]
        [ Element.el
            [ Element.width (Element.px Tokens.postingAccountWidth)
            , Typography.detailLabel
            , Font.semiBold
            , Font.color theme.textMedium
            , Element.alignTop
            ]
            (Element.text (label ++ ":"))
        , Element.paragraph
            [ Element.width Element.fill
            , Typography.detailValue
            , Font.alignRight
            , Font.color theme.textPrimary
            , Common.minWidthZero
            , Common.uiClass "ui-text-wrap"
            ]
            [ Element.text value ]
        ]


maybeDetailRow : Theme -> String -> Maybe String -> List (Element msg)
maybeDetailRow theme label value =
    value |> Maybe.map (\current -> [ detailRow theme label current ]) |> Maybe.withDefault []


postingDetailRow : Theme -> Posting -> Element msg
postingDetailRow theme posting =
    Element.row
        [ Element.width Element.fill
        , Element.spacing Tokens.space12
        , Element.paddingXY Tokens.space0 Tokens.space10
        , Element.alignTop
        , Common.minWidthZero
        ]
        [ Element.row
            [ Element.width Element.fill
            , Element.spacing Tokens.space8
            , Element.alignTop
            , Common.minWidthZero
            ]
            [ Icon.view
                { icon = AccountPresentation.iconForAccount posting.accountType posting.account
                , size = Tokens.iconSizeSmallGlyph
                , color = theme.textMedium
                }
            , Element.column [ Element.width Element.fill, Element.spacing Tokens.space4 ]
                ([ Typography.naturalWrapText
                    [ Typography.detailValue, Font.color theme.textMedium ]
                    ":"
                    posting.account
                 , Typography.mutedCaption theme (Status.toString posting.status)
                 ]
                    ++ maybeCaption theme posting.comment
                    ++ (if List.isEmpty posting.tags then
                            []

                        else
                            [ Typography.mutedCaption theme (tagsText posting.tags) ]
                       )
                )
            ]
        , Element.column
            [ Element.width Element.shrink
            , Element.spacing Tokens.space4
            , Element.alignRight
            , Element.alignTop
            , Font.alignRight
            ]
            (List.map (postingAmountLine theme) posting.amounts)
        ]


postingAmountLine : Theme -> Amount -> Element msg
postingAmountLine theme amount =
    Element.column [ Element.alignRight, Element.spacing Tokens.space2 ]
        [ Element.paragraph
            [ Element.width (Element.fill |> Element.maximum Tokens.narrowContentWidth)
            , Element.alignRight
            , Typography.rowAmount
            , Typography.financeNumber
            , Font.alignRight
            , Font.semiBold
            , Font.color theme.textPrimary
            , Common.minWidthZero
            , Common.uiClass "ui-text-wrap"
            ]
            [ Element.text (amountText amount) ]
        , case amount.cost of
            Just cost ->
                Typography.mutedCaption theme
                    ((if cost.costType == "total" then
                        "Total cost "

                      else
                        "Unit cost "
                     )
                        ++ Formatting.formatFullAmount cost.commodity cost.quantity
                    )

            Nothing ->
                Element.none
        ]


maybeCaption : Theme -> Maybe String -> List (Element msg)
maybeCaption theme value =
    value
        |> Maybe.map (\current -> [ Typography.mutedCaption theme current ])
        |> Maybe.withDefault []


tagsText : List Tag -> String
tagsText =
    List.map (\tag -> tag.name ++ ": " ++ tag.value) >> String.join ", "


amountText : Amount -> String
amountText amount =
    Formatting.formatFullAmount amount.commodity amount.quantity


amountsText : List Amount -> String
amountsText =
    List.map amountText >> String.join ", "
