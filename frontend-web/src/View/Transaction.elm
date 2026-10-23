module View.Transaction exposing
    ( amountsText
    , classificationTone
    , effectLabel
    , transactionDetail
    )

import Data.Date as Date
import Data.Formatting as Formatting
import Data.Status as Status
import Data.Transaction exposing (Amount, Posting, Tag, Transaction, TransactionEffect(..))
import Element exposing (Element)
import Element.Font as Font
import Ui.Chip as Chip
import Ui.Common as Common
import Ui.DesignTokens as Tokens
import Ui.Icon as Icon
import Ui.Surface as Surface
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import View.AccountPresentation as AccountPresentation


classificationTone : TransactionEffect -> Chip.FinancialTone
classificationTone classification =
    case classification of
        TransactionIncome ->
            Chip.FinancialIncome

        IncomeReversal ->
            Chip.FinancialIncome

        TransactionExpense ->
            Chip.FinancialExpense

        ExpenseReversal ->
            Chip.FinancialExpense

        OpeningBalance ->
            Chip.FinancialEquity

        Borrowing ->
            Chip.FinancialBorrowing

        Refinancing ->
            Chip.FinancialBorrowing

        Repayment ->
            Chip.FinancialRepayment

        DebtForgiveness ->
            Chip.FinancialRepayment

        BorrowingReversal ->
            Chip.FinancialRepayment

        AssetAcquisition ->
            Chip.FinancialAsset

        AssetDisposal ->
            Chip.FinancialAsset

        AssetReallocation ->
            Chip.FinancialAsset

        AssetTransfer ->
            Chip.FinancialAsset

        TransactionTransfer ->
            Chip.FinancialNeutral

        Unclassified ->
            Chip.FinancialNeutral

        Unknown _ ->
            Chip.FinancialNeutral


classificationLabel : Transaction -> String
classificationLabel transaction =
    effectLabel transaction.presentation.classification


effectLabel : TransactionEffect -> String
effectLabel effect =
    case effect of
        TransactionIncome ->
            "Income"

        TransactionExpense ->
            "Expense"

        TransactionTransfer ->
            "Transfer"

        OpeningBalance ->
            "Opening balance"

        Borrowing ->
            "Borrowing"

        Repayment ->
            "Repayment"

        AssetAcquisition ->
            "Asset acquisition"

        AssetDisposal ->
            "Asset disposal"

        AssetReallocation ->
            "Asset reallocation"

        AssetTransfer ->
            "Asset transfer"

        Refinancing ->
            "Refinancing"

        DebtForgiveness ->
            "Debt forgiven"

        BorrowingReversal ->
            "Borrowing reversed"

        IncomeReversal ->
            "Income reversed"

        ExpenseReversal ->
            "Expense reversed"

        Unclassified ->
            "Unclassified"

        Unknown _ ->
            "Transfer"


transactionDetail : Theme -> Transaction -> Element msg
transactionDetail theme transaction =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space16 ]
        [ Surface.card theme
            (List.filterMap identity
                [ Just (detailRow theme "Date" (Date.toString transaction.primaryDate))
                , maybeDetailRow
                    theme
                    "Secondary date"
                    (Maybe.map Date.toString transaction.secondaryDate)
                , Just (detailRow theme "Status" (Status.toString transaction.status))
                , maybeDetailRow theme "Code" transaction.code
                , Just (detailRow theme "Description" transaction.description)
                , Just (detailRow theme "Effects" (effectSummary transaction))
                , maybeDetailRow theme "Comment" transaction.comment
                , tagsDetailRow theme transaction
                ]
            )
        , Surface.section theme
            { header = Just (Typography.sectionTitle theme "Postings")
            , body =
                if List.isEmpty transaction.postings then
                    [ Typography.mutedParagraph theme "No postings" ]

                else
                    List.intersperse (Surface.hairline theme)
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
            (Typography.detailLabel
                ++ [ Element.width (Element.px Tokens.postingAccountWidth)
                   , Font.color theme.textMedium
                   , Element.alignTop
                   ]
            )
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


maybeDetailRow : Theme -> String -> Maybe String -> Maybe (Element msg)
maybeDetailRow theme label value =
    Maybe.map (detailRow theme label) value


tagsDetailRow : Theme -> Transaction -> Maybe (Element msg)
tagsDetailRow theme transaction =
    if List.isEmpty transaction.tags then
        Nothing

    else
        Just (detailRow theme "Tags" (tagsText transaction.tags))


postingDetailRow : Theme -> Posting -> Element msg
postingDetailRow theme posting =
    Element.row
        [ Element.width Element.fill
        , Element.spacing Tokens.space12
        , Element.paddingXY Tokens.space0 Tokens.space8
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
