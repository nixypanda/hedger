module Page.Account exposing (Model, Msg(..), clearValue, init, receive, selectValue, toggle, valueView, view)

import Api.Account exposing (AccountNode(..), Response)
import Api.Error as ApiError exposing (ApiError)
import Api.Metadata exposing (Freshness(..))
import Element exposing (Device, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import RemoteData exposing (RemoteData(..))
import Set exposing (Set)
import State.Viewport exposing (Viewport)
import String
import Ui
import Ui.Icon as Icon
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import View.AccountPresentation as AccountPresentation


type Model
    = Model
        { remote : RemoteData Response
        , expanded : Set String
        , selectedValue : Maybe String
        }


type Msg
    = Toggle String


init : Model
init =
    Model
        { remote = NotAsked
        , expanded = Set.empty
        , selectedValue = Nothing
        }


receive : Result ApiError Response -> Model -> Model
receive result (Model model) =
    Model
        { model
            | remote =
                case result of
                    Ok response ->
                        Success response

                    Err error ->
                        Failure error (RemoteData.value model.remote)
        }


toggle : String -> Model -> Model
toggle path (Model model) =
    Model
        { model
            | expanded =
                if Set.member path model.expanded then
                    Set.remove path model.expanded

                else
                    Set.insert path model.expanded
        }


selectValue : String -> Model -> Model
selectValue path (Model model) =
    Model { model | selectedValue = Just path }


clearValue : Model -> Model
clearValue (Model model) =
    Model { model | selectedValue = Nothing }


view : Theme -> Viewport -> { onToggle : String -> msg, onOpen : String -> msg, onValue : String -> msg, onCloseValue : msg } -> Model -> Element msg
view theme viewport actions (Model model) =
    let
        device =
            Element.classifyDevice viewport

        page =
            Element.column
                [ Element.width Element.fill
                , Element.spacing 16
                , Element.htmlAttribute (Html.Attributes.class "page-scroll")
                ]
                [ Ui.pageHeaderBand theme device <|
                    Ui.pageHeader theme
                        { leading = Nothing
                        , title = Typography.pageTitle theme "Accounts"
                        , subtitle = Nothing
                        , actions = []
                        }
                , Ui.pageContainer device [ content theme device actions model ]
                ]

        wideSelected =
            viewport.width >= 1000 && model.selectedValue /= Nothing
    in
    if wideSelected then
        case ( model.selectedValue, RemoteData.value model.remote ) of
            ( Just path, Just response ) ->
                wideLayout theme device actions page path response

            _ ->
                page

    else
        page


wideLayout : Theme -> Device -> { onToggle : String -> msg, onOpen : String -> msg, onValue : String -> msg, onCloseValue : msg } -> Element msg -> String -> Response -> Element msg
wideLayout theme device actions page path response =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.clip
        , Element.htmlAttribute (Html.Attributes.class "accounts-wide-layout")
        ]
        [ Element.el
            [ Element.width (Element.fillPortion 2)
            , Element.height Element.fill
            , Element.scrollbarY
            ]
            page
        , Element.el
            [ Element.width (Element.fillPortion 3)
            , Element.height Element.fill
            , Element.scrollbarY
            ]
            (accountDetailPage theme device (Just actions.onCloseValue) path response)
        ]


content : Theme -> Device -> { onToggle : String -> msg, onOpen : String -> msg, onValue : String -> msg, onCloseValue : msg } -> { remote : RemoteData Response, expanded : Set String, selectedValue : Maybe String } -> Element msg
content theme device actions model =
    case model.remote of
        NotAsked ->
            Ui.loading theme "Loading accounts…"

        Loading Nothing ->
            Ui.card theme [ Ui.skeletonLoading theme ]

        Loading (Just response) ->
            Ui.card theme
                [ Ui.statusBadge theme Ui.StatusPrimary "Refreshing accounts"
                , accountTree theme actions model.expanded response.accounts
                ]

        Success response ->
            stateCard theme response.metadata.freshness (accountTree theme actions model.expanded response.accounts)

        Failure error stale ->
            case stale of
                Just response ->
                    Ui.card theme
                        [ Ui.errorBanner theme ("Refresh failed; showing stale accounts: " ++ ApiError.message error)
                        , accountTree theme actions model.expanded response.accounts
                        ]

                Nothing ->
                    Ui.card theme [ Ui.errorBanner theme ("Accounts failed to load: " ++ ApiError.message error) ]


stateCard : Theme -> Freshness -> Element msg -> Element msg
stateCard theme freshness tree =
    Ui.card theme
        ((case freshness of
            Fresh ->
                []

            Stale ->
                [ Ui.statusBadge theme Ui.StatusWarning "Stale snapshot" ]

            Refreshing ->
                [ Ui.statusBadge theme Ui.StatusPrimary "Refreshing" ]

            Partial ->
                [ Ui.statusBadge theme Ui.StatusWarning "Partial data" ]

            Unavailable ->
                [ Ui.statusBadge theme Ui.StatusDanger "Unavailable" ]
         )
            ++ [ tree ]
        )


accountTree : Theme -> { onToggle : String -> msg, onOpen : String -> msg, onValue : String -> msg, onCloseValue : msg } -> Set String -> List AccountNode -> Element msg
accountTree theme actions expanded accounts =
    if List.isEmpty accounts then
        Ui.emptyState theme "No accounts were found in the journal."

    else
        Element.column [ Element.width Element.fill, Element.spacing 8 ]
            (List.map (accountRow theme actions expanded 0) accounts)


accountRow : Theme -> { onToggle : String -> msg, onOpen : String -> msg, onValue : String -> msg, onCloseValue : msg } -> Set String -> Int -> AccountNode -> Element msg
accountRow theme actions expanded depth (AccountNode account) =
    let
        isExpanded =
            Set.member account.path expanded

        toggleButton =
            if account.hasChildren then
                Input.button
                    [ Element.width (Element.px 28)
                    , Element.height (Element.px 28)
                    , Element.centerY
                    ]
                    { onPress = Just (actions.onToggle account.path)
                    , label =
                        Icon.view
                            { icon =
                                if isExpanded then
                                    Icon.ChevronDown

                                else
                                    Icon.ChevronRight
                            , size = 16
                            , color = theme.textMuted
                            }
                    }

            else
                Element.el [ Element.width (Element.px 28) ] Element.none

        valueAction =
            if hasMultipleCommodities account.subtreeBalances then
                Input.button
                    [ Element.width Element.fill
                    , Element.alignRight
                    , Font.color theme.textPrimary
                    ]
                    { onPress = Just (actions.onValue account.path)
                    , label = balanceText theme account.valuation (Just (commodityCount account.subtreeBalances))
                    }

            else
                balanceText theme account.valuation Nothing

        accountAction =
            Input.button
                [ Element.width (Element.fillPortion 3)
                , Font.color theme.textPrimary
                ]
                { onPress =
                    Just
                        (if account.hasChildren then
                            actions.onToggle account.path

                         else
                            actions.onOpen account.path
                        )
                , label =
                    Element.row [ Element.width Element.fill, Element.spacing 8 ]
                        [ Icon.view
                            { icon = AccountPresentation.iconForAccountName account.path
                            , size = 18
                            , color = theme.primary
                            }
                        , Element.column [ Element.width Element.fill, Element.spacing 2 ]
                            [ Element.text account.name
                            , Element.text (account.accountType |> Maybe.withDefault "account")
                                |> Element.el [ Font.size 12, Font.color theme.textMuted ]
                            ]
                        ]
                }

        row =
            Element.row
                [ Element.width Element.fill
                , Element.paddingXY 12 10
                , Background.color theme.cardBg
                , Border.color theme.divider
                , Border.width 1
                , Border.rounded 8
                , Element.spacing 4
                ]
                [ Element.el [ Element.width (Element.px (depth * 20)) ] Element.none
                , toggleButton
                , accountAction
                , Element.el [ Element.width (Element.fillPortion 1), Element.alignRight ] valueAction
                ]
    in
    Element.column [ Element.width Element.fill, Element.spacing 6 ]
        (row
            :: (if isExpanded then
                    List.map (accountRow theme actions expanded (depth + 1)) account.children

                else
                    []
               )
        )


balanceText : Theme -> Api.Account.Valuation -> Maybe Int -> Element msg
balanceText theme valuation symbolCount =
    let
        amount =
            valuation.subtreeBalances
                |> List.filter (\balance -> balance.commodity == valuation.commodity)
                |> List.head
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing 2
        , Element.alignRight
        , Typography.minWidthZero
        ]
        [ Element.el
            [ Element.alignRight
            , Font.color theme.textPrimary
            , Font.semiBold
            , Typography.minWidthZero
            ]
            (Element.text
                (case amount of
                    Just value ->
                        formatBalance value

                    Nothing ->
                        "—"
                )
            )
        , case symbolCount of
            Just count ->
                Element.el
                    [ Element.alignRight
                    , Font.size 11
                    , Font.color theme.textMuted
                    ]
                    (Element.text (String.fromInt count ++ " symbols"))

            Nothing ->
                Element.none
        ]


formatBalance : { commodity : String, quantity : String } -> String
formatBalance balance =
    case String.toFloat balance.quantity of
        Just quantity ->
            formatAmountDisplay
                (commodityDisplaySymbol balance.commodity)
                quantity

        Nothing ->
            commodityDisplaySymbol balance.commodity ++ " " ++ balance.quantity


formatAmountDisplay : String -> Float -> String
formatAmountDisplay symbol amount =
    let
        absoluteAmount =
            abs amount

        sign =
            if amount < 0 then
                "-"

            else
                ""

        ( value, suffix ) =
            compactValue symbol absoluteAmount
    in
    sign ++ symbol ++ value ++ suffix


compactValue : String -> Float -> ( String, String )
compactValue commodity amount =
    if commodity == "₹" && amount >= 10000000 then
        ( roundedString (amount / 10000000) 1, "Cr" )

    else if commodity == "₹" && amount >= 100000 then
        ( roundedString (amount / 100000) 1, "L" )

    else if amount >= 1000000000 then
        ( roundedString (amount / 1000000000) 1, "B" )

    else if amount >= 1000000 then
        ( roundedString (amount / 1000000) 1, "M" )

    else if amount >= 1000 then
        ( roundedString (amount / 1000) 1, "K" )

    else
        ( roundedString amount 2, "" )


roundedString : Float -> Int -> String
roundedString value decimals =
    let
        factor =
            10 ^ decimals

        rounded =
            toFloat (round (value * toFloat factor)) / toFloat factor

        raw =
            String.fromFloat rounded
    in
    if String.endsWith ".0" raw then
        String.dropRight 2 raw

    else
        raw


formatDate : Api.Account.Date -> String
formatDate (Api.Account.Date date) =
    String.fromInt date.year
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt date.month)
        ++ "-"
        ++ String.padLeft 2 '0' (String.fromInt date.day)


hasMultipleCommodities : List { commodity : String, quantity : String } -> Bool
hasMultipleCommodities balances =
    case balances of
        first :: rest ->
            List.any (\balance -> balance.commodity /= first.commodity) rest

        [] ->
            False


commodityCount : List { commodity : String, quantity : String } -> Int
commodityCount balances =
    balances
        |> List.map .commodity
        |> Set.fromList
        |> Set.size


commodityDisplaySymbol : String -> String
commodityDisplaySymbol commodity =
    case commodity of
        "USD" ->
            "$"

        "INR" ->
            "₹"

        "EUR" ->
            "€"

        "GBP" ->
            "£"

        "JPY" ->
            "¥"

        "CNY" ->
            "¥"

        "BTC" ->
            "₿"

        "CAD" ->
            "CA$"

        "AUD" ->
            "AU$"

        "CHF" ->
            "Fr"

        _ ->
            commodity


valueView : Theme -> Device -> String -> msg -> Model -> Element msg
valueView theme device path onBack (Model model) =
    case RemoteData.value model.remote of
        Just response ->
            accountDetailPage theme device (Just onBack) path response

        Nothing ->
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Background.color theme.pageBg
                , Element.scrollbarY
                , Element.htmlAttribute (Html.Attributes.class "page-scroll account-detail-page")
                ]
                [ Ui.pageHeaderBand theme device <|
                    accountDetailHeader theme device (Just onBack)
                , Ui.pageContainer device [ Ui.card theme [ Ui.loading theme "Loading account value…" ] ]
                ]


accountDetailPage : Theme -> Device -> Maybe msg -> String -> Response -> Element msg
accountDetailPage theme device onBack path response =
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Background.color theme.pageBg
        , Element.scrollbarY
        , Element.htmlAttribute (Html.Attributes.class "page-scroll account-detail-page")
        ]
        [ Ui.pageHeaderBand theme device <|
            accountDetailHeader theme device onBack
        , Ui.pageContainer device [ Ui.card theme (valuePanel theme path response) ]
        ]


accountDetailHeader : Theme -> Device -> Maybe msg -> Element msg
accountDetailHeader theme _ onBack =
    Element.row [ Element.width Element.fill, Element.spacing 8 ]
        [ case onBack of
            Just message ->
                Ui.ghostButton theme { label = "← Back", onPress = message }

            Nothing ->
                Element.none
        , Typography.sectionTitle theme "Account Detail"
        ]


valuePanel : Theme -> String -> Response -> List (Element msg)
valuePanel theme path response =
    case findAccount path response.accounts of
        Nothing ->
            [ Ui.errorBanner theme "That account is no longer present in the current snapshot." ]

        Just (AccountNode account) ->
            [ Element.column [ Element.width Element.fill, Element.spacing 4 ]
                [ Typography.sectionTitle theme account.name
                , Element.text (account.accountType |> Maybe.withDefault "account")
                    |> Element.el [ Font.size 12, Font.color theme.textMuted ]
                ]
            , balanceText theme account.valuation Nothing
            , completenessBadge theme account.valuation.completeness
            , Ui.hairline theme
            , Typography.subsectionTitle theme "Balances included"
            , Element.column [ Element.width Element.fill, Element.spacing 8 ]
                (List.map (detailBalance theme) account.subtreeBalances)
            , Element.text ("Valued as of " ++ formatDate account.valuation.asOf)
                |> Element.el [ Font.size 12, Font.color theme.textMuted ]
            ]


completenessBadge : Theme -> Api.Account.Completeness -> Element msg
completenessBadge theme completeness =
    case completeness of
        Api.Account.Complete ->
            Ui.statusBadge theme Ui.StatusSuccess "Complete valuation"

        Api.Account.Partial ->
            Ui.statusBadge theme Ui.StatusWarning "Partial valuation"

        Api.Account.Unavailable ->
            Ui.statusBadge theme Ui.StatusDanger "Valuation unavailable"


detailBalance : Theme -> { commodity : String, quantity : String } -> Element msg
detailBalance theme balance =
    Element.row
        [ Element.width Element.fill
        , Element.paddingXY 10 8
        , Background.color theme.inputFill
        , Border.rounded 6
        , Element.spacing 8
        ]
        [ Element.text balance.commodity
            |> Element.el [ Element.width (Element.fillPortion 1), Font.color theme.textMedium ]
        , Element.text (formatQuantity balance)
            |> Element.el [ Element.alignRight, Font.semiBold, Font.color theme.textPrimary ]
        ]


formatQuantity : { commodity : String, quantity : String } -> String
formatQuantity balance =
    case String.toFloat balance.quantity of
        Just quantity ->
            formatAmountDisplay (commodityDisplaySymbol balance.commodity) quantity

        Nothing ->
            balance.quantity ++ " " ++ balance.commodity


findAccount : String -> List AccountNode -> Maybe AccountNode
findAccount path nodes =
    case nodes of
        [] ->
            Nothing

        ((AccountNode account) as node) :: rest ->
            if account.path == path then
                Just node

            else
                case findAccount path account.children of
                    Just found ->
                        Just found

                    Nothing ->
                        findAccount path rest
