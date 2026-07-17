module Page.Transactions exposing
    ( Model
    , beginLoading
    , clearSelection
    , init
    , receive
    , reviewFixtures
    , select
    , view
    )

import Api.Error as ApiError exposing (ApiError)
import Api.Metadata exposing (Freshness(..))
import Api.Transaction exposing (Amount, Posting, Response, Tag, Transaction)
import Data.Decimal as Decimal
import Element exposing (Device, Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import RemoteData exposing (RemoteData(..))
import State.Viewport exposing (Viewport)
import Ui
import Ui.Icon as Icon
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import View.AccountPresentation as AccountPresentation


type alias Model =
    { response : RemoteData Response
    , selected : Maybe Transaction
    }


init : Model
init =
    { response = NotAsked, selected = Nothing }


beginLoading : Model -> Model
beginLoading model =
    { model | response = RemoteData.beginLoading model.response }


receive : Result ApiError Response -> Model -> Model
receive result model =
    case result of
        Ok response ->
            { response = Success response
            , selected =
                if revisionChanged model.response response then
                    Nothing

                else
                    model.selected
            }

        Err error ->
            { model | response = Failure error (RemoteData.value model.response) }


select : Transaction -> Model -> Model
select transaction model =
    { model | selected = Just transaction }


clearSelection : Model -> Model
clearSelection model =
    { model | selected = Nothing }


revisionChanged : RemoteData Response -> Response -> Bool
revisionChanged previous current =
    previous
        |> RemoteData.value
        |> Maybe.map (\response -> response.metadata.revision /= current.metadata.revision)
        |> Maybe.withDefault False


view :
    Theme
    -> Viewport
    ->
        { model : Model
        , onRefresh : msg
        , onSelect : Transaction -> msg
        , onBack : msg
        }
    -> Element msg
view theme viewport config =
    let
        device =
            Element.classifyDevice viewport

        response =
            RemoteData.value config.model.response

        wideSelected =
            viewport.width >= 1000 && config.model.selected /= Nothing
    in
    if wideSelected then
        case ( response, config.model.selected ) of
            ( Just current, Just selected ) ->
                wideLayout theme device config current selected

            _ ->
                listPage theme device config

    else if config.model.selected /= Nothing && Ui.isSmall device then
        case config.model.selected of
            Just selected ->
                detailPage theme device (Just config.onBack) selected

            Nothing ->
                listPage theme device config

    else
        listPage theme device config


listPage :
    Theme
    -> Device
    ->
        { model : Model
        , onRefresh : msg
        , onSelect : Transaction -> msg
        , onBack : msg
        }
    -> Element msg
listPage theme device config =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color theme.pageBg
        , Element.scrollbarY
        , Element.htmlAttribute (Html.Attributes.class "page-scroll transactions-page")
        ]
        [ transactionsHeader theme device config.onRefresh
        , Ui.pageContainer device [ remoteContent theme device config ]
        ]


wideLayout :
    Theme
    -> Device
    ->
        { model : Model
        , onRefresh : msg
        , onSelect : Transaction -> msg
        , onBack : msg
        }
    -> Response
    -> Transaction
    -> Element msg
wideLayout theme device config response selected =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.clip
        , Element.htmlAttribute (Html.Attributes.class "transactions-wide-layout")
        ]
        [ Element.column
            [ Element.width (Element.fillPortion 2)
            , Element.height Element.fill
            , Element.scrollbarY
            , Background.color theme.pageBg
            , Element.htmlAttribute (Html.Attributes.class "transactions-master-pane transactions-page")
            ]
            [ transactionsHeader theme device config.onRefresh
            , Ui.pageContainer device
                [ remoteNotice theme config.model.response response
                , transactionCount theme response.transactions
                , transactionsList theme device config.onSelect config.model.selected response.transactions
                ]
            ]
        , Element.el
            [ Element.width (Element.fillPortion 3)
            , Element.height Element.fill
            , Element.scrollbarY
            , Element.htmlAttribute (Html.Attributes.class "transactions-detail-pane")
            ]
            (detailPage theme device (Just config.onBack) selected)
        ]


transactionsHeader : Theme -> Device -> msg -> Element msg
transactionsHeader theme device onRefresh =
    Ui.pageHeaderBand theme device <|
        Ui.pageHeader theme
            { leading = Nothing
            , title = Typography.pageTitle theme "Transactions"
            , subtitle = Just "Browse your ledger activity"
            , actions = [ Ui.ghostButton theme { label = "Refresh", onPress = onRefresh } ]
            }


remoteContent :
    Theme
    -> Device
    ->
        { model : Model
        , onRefresh : msg
        , onSelect : Transaction -> msg
        , onBack : msg
        }
    -> Element msg
remoteContent theme device config =
    case config.model.response of
        NotAsked ->
            Ui.card theme [ Ui.loading theme "Loading transactions…" ]

        Loading Nothing ->
            Ui.card theme [ Ui.skeletonLoading theme ]

        Loading (Just response) ->
            transactionContent theme device config response

        Success response ->
            transactionContent theme device config response

        Failure error stale ->
            case stale of
                Just response ->
                    transactionContent theme device config response

                Nothing ->
                    Ui.card theme [ Ui.errorBanner theme ("Transactions failed to load: " ++ ApiError.message error) ]


transactionContent :
    Theme
    -> Device
    ->
        { model : Model
        , onRefresh : msg
        , onSelect : Transaction -> msg
        , onBack : msg
        }
    -> Response
    -> Element msg
transactionContent theme device config response =
    Element.column [ Element.width Element.fill, Element.spacing 12 ]
        [ remoteNotice theme config.model.response response
        , transactionCount theme response.transactions
        , if List.isEmpty response.transactions then
            Ui.emptyState theme "No transactions loaded"

          else
            transactionsList theme device config.onSelect config.model.selected response.transactions
        ]


remoteNotice : Theme -> RemoteData Response -> Response -> Element msg
remoteNotice theme remote response =
    case remote of
        Loading _ ->
            Ui.statusBadge theme Ui.StatusPrimary "Refreshing transactions"

        Failure error _ ->
            Ui.errorBanner theme ("Refresh failed; showing stale transactions: " ++ ApiError.message error)

        _ ->
            case response.metadata.freshness of
                Fresh ->
                    Element.none

                Refreshing ->
                    Ui.statusBadge theme Ui.StatusPrimary "Refreshing transactions"

                Stale ->
                    Ui.statusBadge theme Ui.StatusWarning "Showing a stale journal snapshot"

                Partial ->
                    Ui.statusBadge theme Ui.StatusWarning "Transaction data is partial"

                Unavailable ->
                    Ui.errorBanner theme "Transaction data is unavailable."


transactionCount : Theme -> List Transaction -> Element msg
transactionCount theme transactions =
    Typography.mutedBody theme (String.fromInt (List.length transactions) ++ " transactions")


transactionsList : Theme -> Device -> (Transaction -> msg) -> Maybe Transaction -> List Transaction -> Element msg
transactionsList theme device onSelect selected transactions =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 12
        , Element.htmlAttribute (Html.Attributes.id "transaction-list")
        ]
        (List.map (monthSection theme device onSelect selected) (monthGroups transactions))


monthSection : Theme -> Device -> (Transaction -> msg) -> Maybe Transaction -> ( String, List Transaction ) -> Element msg
monthSection theme device onSelect selected ( month, transactions ) =
    Element.column [ Element.width Element.fill, Element.spacing 4 ]
        (Element.el [ Element.paddingEach { top = 8, right = 0, bottom = 4, left = 4 } ]
            (Typography.subsectionTitle theme (monthLabel month))
            :: List.map (transactionRow theme device onSelect selected) transactions
        )


transactionRow : Theme -> Device -> (Transaction -> msg) -> Maybe Transaction -> Transaction -> Element msg
transactionRow theme device onSelect selected transaction =
    let
        rowContent =
            Element.column
                (Ui.rowSurfaceAttrs theme (selected == Just transaction) ++ [ Element.spacing 6 ])
                [ if Ui.isSmall device then
                    phonePrimaryRow theme transaction

                  else
                    desktopPrimaryRow theme transaction
                , Element.column
                    [ Element.width Element.fill
                    , Element.spacing 3
                    , Element.paddingEach { top = 0, right = 0, bottom = 0, left = 12 }
                    ]
                    (List.map (postingPreviewRow theme) (List.take 5 transaction.postings))
                , let
                    omitted =
                        max 0 (List.length transaction.postings - 5)
                  in
                  if omitted > 0 then
                    Element.el [ Element.paddingEach { top = 0, right = 0, bottom = 0, left = 12 } ]
                        (Typography.mutedCaption theme ("+" ++ String.fromInt omitted ++ " more"))

                  else
                    Element.none
                ]
    in
    Input.button [ Element.width Element.fill, Element.padding 0 ]
        { onPress = Just (onSelect transaction), label = rowContent }


desktopPrimaryRow : Theme -> Transaction -> Element msg
desktopPrimaryRow theme transaction =
    Element.row [ Element.width Element.fill, Element.spacing 8 ]
        [ Element.el [ Typography.rowDate, Font.color theme.textMuted, Element.width (Element.px 90) ]
            (Element.text transaction.primaryDate)
        , Element.el
            [ Typography.rowTitle
            , Font.semiBold
            , Element.width Element.fill
            , Font.color theme.textPrimary
            , Typography.minWidthZero
            , Typography.uiClass "ui-text-ellipsis"
            ]
            (Element.text transaction.description)
        , trailingSummary theme transaction
        ]


phonePrimaryRow : Theme -> Transaction -> Element msg
phonePrimaryRow theme transaction =
    Element.column [ Element.width Element.fill, Element.spacing 3 ]
        [ Element.el [ Typography.rowDate, Font.color theme.textMuted ] (Element.text transaction.primaryDate)
        , Element.row [ Element.width Element.fill, Element.spacing 8 ]
            [ Element.paragraph
                [ Typography.rowTitle
                , Font.semiBold
                , Element.width Element.fill
                , Font.color theme.textPrimary
                , Typography.minWidthZero
                , Typography.uiClass "ui-text-wrap"
                ]
                [ Element.text transaction.description ]
            , trailingSummary theme transaction
            ]
        ]


trailingSummary : Theme -> Transaction -> Element msg
trailingSummary theme transaction =
    Element.column [ Element.alignRight, Element.spacing 2 ]
        [ Element.el
            [ Typography.rowAmount
            , Font.semiBold
            , Font.color theme.textPrimary
            , Element.alignRight
            , Element.width (Element.shrink |> Element.maximum 180)
            , Typography.uiClass "ui-text-ellipsis"
            , Typography.financeNumber
            ]
            (Element.text (primaryAmount transaction))
        , Element.el [ Element.alignRight ]
            (Ui.financialBadge theme (classificationTone transaction.presentation.classification) (classificationLabel transaction))
        ]


classificationTone : String -> Ui.FinancialTone
classificationTone classification =
    case classification of
        "income" ->
            Ui.FinancialIncome

        "expense" ->
            Ui.FinancialExpense

        "opening_balance" ->
            Ui.FinancialEquity

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

        _ ->
            "Transfer"


primaryAmount : Transaction -> String
primaryAmount transaction =
    case transaction.presentation.classification of
        "opening_balance" ->
            let
                count =
                    Maybe.withDefault 0 transaction.presentation.openingAccountCount
            in
            String.fromInt count
                ++ (if count == 1 then
                        " account"

                    else
                        " accounts"
                   )

        _ ->
            transaction.presentation.totalAmounts
                |> List.head
                |> Maybe.map (\amount -> Decimal.toString amount.quantity ++ " " ++ amount.commodity)
                |> Maybe.withDefault ""


postingPreviewRow : Theme -> Posting -> Element msg
postingPreviewRow theme posting =
    Element.row [ Element.width Element.fill, Element.spacing 4 ]
        [ Icon.view { icon = AccountPresentation.iconForAccountName posting.account, size = 14, color = theme.textMuted }
        , Element.paragraph
            [ Typography.rowMetadata
            , Font.color theme.textMuted
            , Element.width Element.fill
            , Typography.minWidthZero
            , Typography.uiClass "ui-text-wrap"
            ]
            [ Element.text posting.account ]
        , Element.el
            [ Typography.rowMetadata
            , Font.color theme.textMuted
            , Element.alignRight
            , Element.width (Element.shrink |> Element.maximum 180)
            , Typography.minWidthZero
            , Typography.uiClass "ui-text-ellipsis"
            , Typography.financeNumber
            ]
            (Element.text (amountsText posting.amounts))
        ]


detailPage : Theme -> Device -> Maybe msg -> Transaction -> Element msg
detailPage theme device onBack transaction =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color theme.pageBg
        , Element.scrollbarY
        , Element.htmlAttribute (Html.Attributes.class "page-scroll transaction-detail-page")
        , Element.htmlAttribute (Html.Attributes.id "transaction-detail")
        ]
        [ Ui.pageHeaderBand theme device <|
            Element.row [ Element.width Element.fill, Element.spacing 8 ]
                [ case onBack of
                    Just message ->
                        Ui.ghostButton theme { label = "← Back", onPress = message }

                    Nothing ->
                        Element.none
                , Typography.sectionTitle theme "Transaction Detail"
                ]
        , Ui.pageContainer device [ transactionDetail theme transaction ]
        ]


transactionDetail : Theme -> Transaction -> Element msg
transactionDetail theme transaction =
    Element.column [ Element.width Element.fill, Element.spacing 16 ]
        [ Ui.card theme
            (List.concat
                [ [ detailRow theme "Date" transaction.primaryDate ]
                , maybeDetailRow theme "Secondary date" transaction.secondaryDate
                , [ detailRow theme "Status" (statusLabel transaction.status) ]
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
                    List.intersperse (Ui.hairline theme) (List.map (postingDetailRow theme) transaction.postings)
            , footer = Nothing
            }
        ]


detailRow : Theme -> String -> String -> Element msg
detailRow theme label value =
    Element.row [ Element.width Element.fill, Element.spacing 16, Element.alignTop, Typography.minWidthZero ]
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
    Element.row [ Element.width Element.fill, Element.spacing 12, Element.paddingXY 0 10, Element.alignTop, Typography.minWidthZero ]
        [ Element.row [ Element.width Element.fill, Element.spacing 8, Element.alignTop, Typography.minWidthZero ]
            [ Icon.view { icon = AccountPresentation.iconForAccountName posting.account, size = 16, color = theme.textMedium }
            , Element.column [ Element.width Element.fill, Element.spacing 4 ]
                ([ Typography.naturalWrapText [ Typography.detailValue, Font.color theme.textMedium ] ":" posting.account
                 , Typography.mutedCaption theme (statusLabel posting.status)
                 ]
                    ++ maybeCaption theme posting.comment
                    ++ (if List.isEmpty posting.tags then
                            []

                        else
                            [ Typography.mutedCaption theme (tagsText posting.tags) ]
                       )
                )
            ]
        , Element.column [ Element.width Element.shrink, Element.spacing 4, Element.alignRight, Element.alignTop, Font.alignRight ]
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


statusLabel : String -> String
statusLabel status =
    case status of
        "cleared" ->
            "Cleared"

        "pending" ->
            "Pending"

        _ ->
            "Unmarked"


amountText : Amount -> String
amountText amount =
    Decimal.toString amount.quantity ++ " " ++ amount.commodity


amountsText : List Amount -> String
amountsText =
    List.map amountText >> String.join ", "


monthGroups : List Transaction -> List ( String, List Transaction )
monthGroups transactions =
    case transactions of
        [] ->
            []

        first :: _ ->
            let
                key =
                    String.left 7 first.primaryDate

                ( matching, remaining ) =
                    splitMonth key transactions
            in
            ( key, matching ) :: monthGroups remaining


splitMonth : String -> List Transaction -> ( List Transaction, List Transaction )
splitMonth key transactions =
    case transactions of
        [] ->
            ( [], [] )

        first :: rest ->
            if String.left 7 first.primaryDate == key then
                let
                    ( matching, remaining ) =
                        splitMonth key rest
                in
                ( first :: matching, remaining )

            else
                ( [], transactions )


monthLabel : String -> String
monthLabel value =
    case String.split "-" value of
        [ year, month ] ->
            monthName month ++ " " ++ year

        _ ->
            value


monthName : String -> String
monthName month =
    case month of
        "01" ->
            "January"

        "02" ->
            "February"

        "03" ->
            "March"

        "04" ->
            "April"

        "05" ->
            "May"

        "06" ->
            "June"

        "07" ->
            "July"

        "08" ->
            "August"

        "09" ->
            "September"

        "10" ->
            "October"

        "11" ->
            "November"

        "12" ->
            "December"

        _ ->
            month


reviewFixtures : Theme -> msg -> Element msg
reviewFixtures theme message =
    Element.column [ Element.width Element.fill, Element.spacing 20 ]
        [ Typography.sectionTitle theme "Transactions · eLedger wide master/detail"
        , Element.el [ Element.width Element.fill, Element.height (Element.px 720) ]
            (view theme
                { width = 1180, height = 720 }
                { model = { response = Success sampleResponse, selected = List.head sampleResponse.transactions }
                , onRefresh = message
                , onSelect = always message
                , onBack = message
                }
            )
        , Typography.sectionTitle theme "Transactions · eLedger narrow list"
        , Element.el [ Element.width (Element.px 360), Element.height (Element.px 680), Element.centerX ]
            (view theme
                { width = 360, height = 680 }
                { model = { response = Success sampleResponse, selected = Nothing }
                , onRefresh = message
                , onSelect = always message
                , onBack = message
                }
            )
        ]


sampleResponse : Response
sampleResponse =
    { metadata = { revision = Just "review", freshness = Fresh, warnings = [] }
    , transactions =
        [ { primaryDate = "2024-04-02"
          , secondaryDate = Just "2024-04-03"
          , status = "cleared"
          , code = Just "UTIL"
          , description = "Utilities"
          , comment = Just "Monthly household bills"
          , tags = [ { name = "category", value = "household" } ]
          , postings =
                [ { account = "expenses:utilities"
                  , status = "cleared"
                  , comment = Just "Automatic payment"
                  , tags = []
                  , amounts = [ sampleAmount "3000" "INR" ]
                  }
                , { account = "assets:cash:checking"
                  , status = "unmarked"
                  , comment = Nothing
                  , tags = []
                  , amounts = [ sampleAmount "-3000" "INR" ]
                  }
                ]
          , presentation =
                { classification = "expense"
                , fromAccounts = [ "assets" ]
                , toAccounts = [ "expenses" ]
                , totalAmounts = [ { commodity = "INR", quantity = decimal "3000" } ]
                , openingAccountCount = Nothing
                }
          }
        ]
    }


sampleAmount : String -> String -> Amount
sampleAmount quantity commodity =
    { commodity = commodity, quantity = decimal quantity, cost = Nothing }


decimal : String -> Decimal.Decimal
decimal value =
    Decimal.fromString value |> Result.withDefault Decimal.zero
