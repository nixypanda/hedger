module Page.Account exposing
    ( Model
    , Msg(..)
    , OutMsg(..)
    , clearValue
    , fetchAccounts
    , init
    , initWithValueSelected
    , receive
    , selectValue
    , toggle
    , update
    , valueView
    , view
    )

import Api.Account exposing (AccountNode(..), Response)
import Api.Client as Api
import Api.Error as ApiError exposing (ApiError)
import Api.Metadata exposing (Freshness(..))
import Element exposing (Device, Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Lib.Time as TimeExtra
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))
import Set exposing (Set)
import State.Viewport exposing (Viewport)
import String
import Ui
import Ui.Icon as Icon
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import View.AccountPresentation as AccountPresentation
import View.Formatting as Formatting
import View.States as States


type Model
    = Model
        { remote : RemoteData Response
        , expanded : Set String
        , selectedValue : Maybe String
        }


type Msg
    = ToggleAccount String
    | OpenAccount String
    | OpenAccountValue String
    | CloseAccountValue
    | BackToAccounts
    | Receive (Result ApiError Response)


type OutMsg
    = NoOutMsg
    | Navigate Route


type alias UpdateProps =
    { viewport : Viewport }


init : Model
init =
    Model
        { remote = NotAsked
        , expanded = Set.empty
        , selectedValue = Nothing
        }


initWithValueSelected : String -> Model
initWithValueSelected accountName =
    Model
        { remote = NotAsked
        , expanded = Set.empty
        , selectedValue = Just accountName
        }


fetchAccounts : Cmd Msg
fetchAccounts =
    Api.getAccounts Nothing Receive


update : UpdateProps -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update props msg (Model model) =
    case msg of
        ToggleAccount path ->
            ( toggle path (Model model), Cmd.none, NoOutMsg )

        OpenAccount path ->
            ( Model model, Cmd.none, Navigate (Account path) )

        OpenAccountValue path ->
            if Ui.isSmall (Element.classifyDevice props.viewport) then
                ( Model model, Cmd.none, Navigate (AccountValue path) )

            else
                ( selectValue path (Model model), Cmd.none, NoOutMsg )

        CloseAccountValue ->
            ( clearValue (Model model), Cmd.none, NoOutMsg )

        BackToAccounts ->
            ( clearValue (Model model), Cmd.none, Navigate Accounts )

        Receive result ->
            ( receive result (Model model), Cmd.none, NoOutMsg )


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


view :
    Theme
    -> Viewport
    -> Model
    -> Element Msg
view theme viewport (Model model) =
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
                , Ui.pageContainer device [ content theme device model ]
                ]

        wideSelected =
            viewport.width >= 1000 && model.selectedValue /= Nothing
    in
    if wideSelected then
        case ( model.selectedValue, RemoteData.value model.remote ) of
            ( Just path, Just response ) ->
                wideLayout theme device page path response

            _ ->
                page

    else
        page


wideLayout :
    Theme
    -> Device
    -> Element Msg
    -> String
    -> Response
    -> Element Msg
wideLayout theme device page path response =
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
            (accountDetailPage theme device (Just BackToAccounts) path response)
        ]


content :
    Theme
    -> Device
    ->
        { remote : RemoteData Response
        , expanded : Set String
        , selectedValue : Maybe String
        }
    -> Element Msg
content theme device model =
    case model.remote of
        NotAsked ->
            Ui.loading theme "Loading accounts…"

        Loading Nothing ->
            Ui.card theme [ Ui.skeletonLoading theme ]

        Loading (Just response) ->
            Ui.card theme
                [ Ui.statusBadge theme Ui.StatusPrimary "Refreshing accounts"
                , accountTree theme model.expanded response.accounts
                ]

        Success response ->
            stateCard theme
                response.metadata.freshness
                (accountTree theme model.expanded response.accounts)

        Failure error stale ->
            case stale of
                Just response ->
                    Ui.card theme
                        [ Ui.errorBanner theme
                            ("Refresh failed; showing stale accounts: " ++ ApiError.message error)
                        , accountTree theme model.expanded response.accounts
                        ]

                Nothing ->
                    Ui.card theme
                        [ Ui.errorBanner theme
                            ("Accounts failed to load: " ++ ApiError.message error)
                        ]


stateCard : Theme -> Freshness -> Element msg -> Element msg
stateCard theme freshness tree =
    Ui.card theme
        [ States.freshnessBanner theme freshness
        , tree
        ]


accountTree :
    Theme
    -> Set String
    -> List AccountNode
    -> Element Msg
accountTree theme expanded accounts =
    if List.isEmpty accounts then
        Ui.emptyState theme "No accounts were found in the journal."

    else
        Element.column [ Element.width Element.fill, Element.spacing 8 ]
            (List.map (accountRow theme expanded 0) accounts)


accountRow :
    Theme
    -> Set String
    -> Int
    -> AccountNode
    -> Element Msg
accountRow theme expanded depth (AccountNode account) =
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
                    { onPress = Just (ToggleAccount account.path)
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
                    { onPress = Just (OpenAccountValue account.path)
                    , label =
                        balanceText theme
                            account.valuation
                            (Just (commodityCount account.subtreeBalances))
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
                            ToggleAccount account.path

                         else
                            OpenAccount account.path
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
                , Element.el
                    [ Element.width (Element.fillPortion 1)
                    , Element.alignRight
                    ]
                    valueAction
                ]
    in
    Element.column [ Element.width Element.fill, Element.spacing 6 ]
        (row
            :: (if isExpanded then
                    List.map (accountRow theme expanded (depth + 1)) account.children

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
                        Formatting.formatBalance value

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


valueView : Theme -> Device -> String -> Model -> Element Msg
valueView theme device path (Model model) =
    case RemoteData.value model.remote of
        Just response ->
            accountDetailPage theme device (Just BackToAccounts) path response

        Nothing ->
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Background.color theme.pageBg
                , Element.scrollbarY
                , Element.htmlAttribute (Html.Attributes.class "page-scroll account-detail-page")
                ]
                [ Ui.pageHeaderBand theme device <|
                    accountDetailHeader theme device (Just BackToAccounts)
                , Ui.pageContainer device [ Ui.card theme [ Ui.loading theme "Loading account value…" ] ]
                ]


accountDetailPage : Theme -> Device -> Maybe Msg -> String -> Response -> Element Msg
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


accountDetailHeader : Theme -> Device -> Maybe Msg -> Element Msg
accountDetailHeader theme _ onBack =
    Element.row [ Element.width Element.fill, Element.spacing 8 ]
        [ case onBack of
            Just message ->
                Ui.ghostButton theme { label = "← Back", onPress = message }

            Nothing ->
                Element.none
        , Typography.sectionTitle theme "Account Detail"
        ]


valuePanel : Theme -> String -> Response -> List (Element Msg)
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
            , Element.text
                (let
                    (Api.Account.Date date) =
                        account.valuation.asOf
                 in
                 "Valued as of " ++ TimeExtra.formatDate date
                )
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
            Formatting.formatAmountDisplay (Formatting.commodityDisplaySymbol balance.commodity) quantity

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
