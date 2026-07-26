module View.Transaction exposing (amountsText, classificationLabel, classificationTone, transactionDetail)

import Api.Transaction exposing (Amount, Posting, Tag, Transaction)
import Data.Date as Date
import Data.Decimal as Decimal
import Data.Status as Status
import Element exposing (Element)
import Element.Font as Font
import Ui
import Ui.Icon as Icon
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import View.AccountPresentation as AccountPresentation


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


transactionDetail : Theme -> Transaction -> Element msg
transactionDetail theme transaction =
    Element.column [ Element.width Element.fill, Element.spacing 16 ]
        [ Ui.card theme
            (List.concat
                [ [ detailRow theme "Date" (Date.toString transaction.primaryDate) ]
                , maybeDetailRow theme "Secondary date" (Maybe.map Date.toString transaction.secondaryDate)
                , [ detailRow theme "Status" (Status.toString transaction.status) ]
                , maybeDetailRow theme "Code" transaction.code
                , [ detailRow theme "Description" transaction.description ]
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


detailRow : Theme -> String -> String -> Element msg
detailRow theme label value =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 16
        , Element.alignTop
        , Typography.minWidthZero
        ]
        [ Element.el
            [ Element.width (Element.px 112)
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
            , Typography.minWidthZero
            , Typography.uiClass "ui-text-wrap"
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
        , Element.spacing 12
        , Element.paddingXY 0 10
        , Element.alignTop
        , Typography.minWidthZero
        ]
        [ Element.row
            [ Element.width Element.fill
            , Element.spacing 8
            , Element.alignTop
            , Typography.minWidthZero
            ]
            [ Icon.view
                { icon = AccountPresentation.iconForAccountName posting.account
                , size = 16
                , color = theme.textMedium
                }
            , Element.column [ Element.width Element.fill, Element.spacing 4 ]
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
            , Element.spacing 4
            , Element.alignRight
            , Element.alignTop
            , Font.alignRight
            ]
            (List.map (postingAmountLine theme) posting.amounts)
        ]


postingAmountLine : Theme -> Amount -> Element msg
postingAmountLine theme amount =
    Element.column [ Element.alignRight, Element.spacing 2 ]
        [ Element.paragraph
            [ Element.width (Element.fill |> Element.maximum 180)
            , Element.alignRight
            , Typography.rowAmount
            , Typography.financeNumber
            , Font.alignRight
            , Font.semiBold
            , Font.color theme.textPrimary
            , Typography.minWidthZero
            , Typography.uiClass "ui-text-wrap"
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
                        ++ Decimal.toString cost.quantity
                        ++ " "
                        ++ cost.commodity
                    )

            Nothing ->
                Element.none
        ]


maybeCaption : Theme -> Maybe String -> List (Element msg)
maybeCaption theme value =
    value |> Maybe.map (\current -> [ Typography.mutedCaption theme current ]) |> Maybe.withDefault []


tagsText : List Tag -> String
tagsText =
    List.map (\tag -> tag.name ++ ": " ++ tag.value) >> String.join ", "


amountText : Amount -> String
amountText amount =
    Decimal.toString amount.quantity ++ " " ++ amount.commodity


amountsText : List Amount -> String
amountsText =
    List.map amountText >> String.join ", "
