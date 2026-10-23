module Page.Account exposing
    ( Model
    , Msg(..)
    , OutMsg(..)
    , beginLoading
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

import Api.Account exposing (Response)
import Api.Client as Api
import Api.Error as ApiError exposing (ApiError)
import Data.Account as Account exposing (AccountNode(..), AccountPath)
import Data.AccountType as AccountType
import Data.Completeness as Completeness exposing (Completeness)
import Data.Date as Date
import Data.Formatting as Formatting
import Data.Freshness exposing (Freshness)
import Data.Quantity as Quantity exposing (Quantity)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Lib.Time as TimeExtra
import Route exposing (Route(..))
import Set exposing (Set)
import String
import Ui.Button as Button
import Ui.Chip as Chip
import Ui.Common as Common
import Ui.DesignTokens as Tokens
import Ui.Env exposing (Env)
import Ui.Feedback as Feedback
import Ui.Icon as Icon
import Ui.Page as Page
import Ui.Skeleton as Skeleton
import Ui.Surface as Surface
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import Ui.Viewport as Viewport
import Ui.Workspace as Workspace
import View.AccountPresentation as AccountPresentation
import View.States as States



-- MODEL


type Model
    = Model
        { accounts : AccountsState
        , expanded : Set String
        , detail : DetailState
        }


type AccountsState
    = NotRequested
    | Loading
    | Loaded Response
    | Refreshing Response
    | Failed ApiError
    | FailedWithStale ApiError Response


type DetailState
    = NoDetail
    | WaitingForAccount AccountPath
    | ShowingAccount AccountNode
    | AccountUnavailable AccountPath


init : Model
init =
    Model
        { accounts = NotRequested
        , expanded = Set.empty
        , detail = NoDetail
        }


initWithValueSelected : String -> Model
initWithValueSelected accountName =
    Model
        { accounts = Loading
        , expanded = Set.empty
        , detail = WaitingForAccount (Account.accountPath accountName)
        }


beginLoading : Model -> Model
beginLoading (Model model) =
    Model
        { model
            | accounts =
                case model.accounts of
                    Loaded response ->
                        Refreshing response

                    Refreshing response ->
                        Refreshing response

                    FailedWithStale _ response ->
                        Refreshing response

                    _ ->
                        Loading
        }



-- UPDATE


type Msg
    = ToggleAccount AccountPath
    | OpenAccount AccountPath
    | OpenAccountValue AccountPath
    | CloseAccountValue
    | BackToAccounts
    | Receive (Result ApiError Response)


type OutMsg
    = NoOutMsg
    | Navigate Route


type alias UpdateProps =
    { env : Env }


fetchAccounts : Cmd Msg
fetchAccounts =
    Api.getAccounts Nothing Receive


update : UpdateProps -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update props msg (Model model) =
    case msg of
        ToggleAccount path ->
            ( toggle path (Model model), Cmd.none, NoOutMsg )

        OpenAccount path ->
            ( Model model, Cmd.none, Navigate (Account (Account.accountPathToString path)) )

        OpenAccountValue path ->
            if Viewport.usesMasterDetail props.env.viewport then
                ( selectValue path (Model model), Cmd.none, NoOutMsg )

            else
                ( Model model
                , Cmd.none
                , Navigate (AccountValue (Account.accountPathToString path))
                )

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
            | accounts =
                case result of
                    Ok response ->
                        Loaded response

                    Err error ->
                        case model.accounts of
                            Loaded response ->
                                FailedWithStale error response

                            Refreshing response ->
                                FailedWithStale error response

                            FailedWithStale _ response ->
                                FailedWithStale error response

                            _ ->
                                Failed error
            , detail =
                case result of
                    Ok response ->
                        resolveDetail response model.detail

                    Err _ ->
                        model.detail
        }


resolveDetail : Response -> DetailState -> DetailState
resolveDetail response detail =
    case detail of
        WaitingForAccount path ->
            findAccount path response.accounts
                |> Maybe.map ShowingAccount
                |> Maybe.withDefault (AccountUnavailable path)

        ShowingAccount (AccountNode account) ->
            findAccount account.path response.accounts
                |> Maybe.map ShowingAccount
                |> Maybe.withDefault (AccountUnavailable account.path)

        AccountUnavailable path ->
            case findAccount path response.accounts of
                Just account ->
                    ShowingAccount account

                Nothing ->
                    AccountUnavailable path

        NoDetail ->
            NoDetail


toggle : AccountPath -> Model -> Model
toggle path (Model model) =
    Model
        { model
            | expanded =
                if Set.member (Account.accountPathToString path) model.expanded then
                    Set.remove (Account.accountPathToString path) model.expanded

                else
                    Set.insert (Account.accountPathToString path) model.expanded
        }


selectValue : AccountPath -> Model -> Model
selectValue path (Model model) =
    Model
        { model
            | detail =
                case model.accounts of
                    Loaded response ->
                        findAccount path response.accounts
                            |> Maybe.map ShowingAccount
                            |> Maybe.withDefault (AccountUnavailable path)

                    _ ->
                        WaitingForAccount path
        }


clearValue : Model -> Model
clearValue (Model model) =
    Model { model | detail = NoDetail }



-- VIEW


view :
    Env
    -> Model
    -> Element Msg
view env (Model model) =
    let
        theme =
            env.theme

        viewport =
            env.viewport

        page =
            Page.scaffold theme
                { header =
                    { leading = Nothing
                    , title = Typography.pageTitle theme "Accounts"
                    , subtitle = Nothing
                    , actions = []
                    }
                , body = [ content theme model ]
                }

        wideSelected =
            Viewport.usesMasterDetail viewport && detailAccount model.detail /= Nothing
    in
    if wideSelected then
        case detailAccount model.detail of
            Just account ->
                Workspace.view env
                    [ { key = "accounts"
                      , width = Workspace.Portion Tokens.layoutPortionDouble
                      , content = page
                      }
                    , { key = "account-detail"
                      , width = Workspace.Portion Tokens.layoutPortionTriple
                      , content = accountDetailPage theme (Just BackToAccounts) account
                      }
                    ]

            _ ->
                page

    else
        page


content :
    Theme
    ->
        { accounts : AccountsState
        , expanded : Set String
        , detail : DetailState
        }
    -> Element Msg
content theme model =
    case model.accounts of
        NotRequested ->
            Feedback.loading theme "Loading accounts…"

        Loading ->
            Surface.card theme
                [ Skeleton.listRow theme
                , Surface.hairline theme
                , Skeleton.listRow theme
                , Surface.hairline theme
                , Skeleton.listRow theme
                , Surface.hairline theme
                , Skeleton.listRow theme
                , Surface.hairline theme
                , Skeleton.listRow theme
                ]

        Refreshing response ->
            Surface.card theme
                [ Chip.statusBadge theme Chip.StatusPrimary "Refreshing accounts"
                , accountTree theme model.expanded response.accounts
                ]

        Loaded response ->
            stateCard theme
                response.metadata.freshness
                (accountTree theme model.expanded response.accounts)

        FailedWithStale error response ->
            Surface.card theme
                [ Feedback.errorBanner theme
                    ("Refresh failed; showing stale accounts: " ++ ApiError.message error)
                , accountTree theme model.expanded response.accounts
                ]

        Failed error ->
            Surface.card theme
                [ Feedback.errorBanner theme
                    ("Accounts failed to load: " ++ ApiError.message error)
                ]


stateCard : Theme -> Freshness -> Element msg -> Element msg
stateCard theme freshness tree =
    Surface.card theme
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
        Feedback.emptyState theme "No accounts were found in the journal."

    else
        Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
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
            Set.member (Account.accountPathToString account.path) expanded

        toggleButton =
            if not (List.isEmpty account.children) then
                Input.button
                    [ Element.width (Element.px Tokens.minimumTouchTarget)
                    , Element.height (Element.px Tokens.minimumTouchTarget)
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
                            , size = Tokens.iconSizeSmallGlyph
                            , color = theme.textMuted
                            }
                    }

            else
                Element.el [ Element.width (Element.px Tokens.minimumTouchTarget) ] Element.none

        valueAction =
            if Quantity.hasMultipleCommodities account.subtreeBalances then
                Input.button
                    [ Element.width Element.fill
                    , Common.touchTargetHeight
                    , Element.alignRight
                    , Font.color theme.textPrimary
                    ]
                    { onPress = Just (OpenAccountValue account.path)
                    , label =
                        balanceText theme
                            account.valuation
                            (Just (Quantity.commodityCount account.subtreeBalances))
                    }

            else
                balanceText theme account.valuation Nothing

        accountAction =
            Input.button
                [ Element.width (Element.fillPortion Tokens.layoutPortionTriple)
                , Common.touchTargetHeight
                , Font.color theme.textPrimary
                ]
                { onPress =
                    Just
                        (if not (List.isEmpty account.children) then
                            ToggleAccount account.path

                         else
                            OpenAccount account.path
                        )
                , label =
                    Element.row [ Element.width Element.fill, Element.spacing Tokens.space8 ]
                        [ Icon.view
                            { icon =
                                AccountPresentation.iconForAccount
                                    account.accountType
                                    (Account.accountPathToString account.path)
                            , size = Tokens.iconSizeCaption
                            , color = theme.primary
                            }
                        , Element.column
                            [ Element.width Element.fill
                            , Element.spacing Tokens.space2
                            ]
                            [ Element.text account.name
                            , Element.text
                                (account.accountType
                                    |> Maybe.map AccountType.toString
                                    |> Maybe.withDefault "account"
                                )
                                |> Element.el [ Typography.caption, Font.color theme.textMuted ]
                            ]
                        ]
                }

        row =
            Element.row
                [ Element.width Element.fill
                , Element.paddingXY Tokens.space12 Tokens.space8
                , Background.color theme.cardBg
                , Border.color theme.divider
                , Border.width Tokens.borderHairline
                , Border.rounded Tokens.radiusSmall
                , Element.spacing Tokens.space4
                ]
                [ Element.el
                    [ Element.width (Element.px (depth * Tokens.treeIndentWidth)) ]
                    Element.none
                , toggleButton
                , accountAction
                , Element.el
                    [ Element.width (Element.fillPortion Tokens.layoutPortionSingle)
                    , Element.alignRight
                    ]
                    valueAction
                ]
    in
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
        (row
            :: (if isExpanded then
                    List.map (accountRow theme expanded (depth + 1)) account.children

                else
                    []
               )
        )


balanceText : Theme -> Account.Valuation -> Maybe Int -> Element msg
balanceText theme valuation symbolCount =
    let
        amount =
            valuation.subtreeBalances
                |> List.filter (\balance -> balance.commodity == valuation.commodity)
                |> List.head
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing Tokens.space2
        , Element.alignRight
        , Common.minWidthZero
        ]
        [ Element.el
            [ Element.alignRight
            , Font.color theme.textPrimary
            , Font.semiBold
            , Common.minWidthZero
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
                    , Typography.caption
                    , Font.color theme.textMuted
                    ]
                    (Element.text (String.fromInt count ++ " symbols"))

            Nothing ->
                Element.none
        ]


valueView : Env -> String -> Model -> Element Msg
valueView env path (Model model) =
    let
        theme =
            env.theme
    in
    case model.accounts of
        Loaded response ->
            detailForPath theme path response

        Refreshing response ->
            detailForPath theme path response

        FailedWithStale _ response ->
            detailForPath theme path response

        _ ->
            Page.scaffold theme
                { header = accountDetailHeader theme (Just BackToAccounts)
                , body =
                    [ Surface.card theme [ Feedback.loading theme "Loading account value…" ] ]
                }


accountDetailPage : Theme -> Maybe Msg -> AccountNode -> Element Msg
accountDetailPage theme onBack account =
    Page.scaffold theme
        { header = accountDetailHeader theme onBack
        , body = [ Surface.card theme (valuePanel theme account) ]
        }


accountDetailHeader : Theme -> Maybe Msg -> Page.Header Msg
accountDetailHeader theme onBack =
    { leading =
        Maybe.map
            (\message -> Button.backButton theme { onPress = message })
            onBack
    , title = Typography.pageTitle theme "Account Detail"
    , subtitle = Nothing
    , actions = []
    }


valuePanel : Theme -> AccountNode -> List (Element Msg)
valuePanel theme (AccountNode account) =
    [ Element.column [ Element.width Element.fill, Element.spacing Tokens.space4 ]
        [ Typography.sectionTitle theme account.name
        , Element.text
            (account.accountType
                |> Maybe.map AccountType.toString
                |> Maybe.withDefault "account"
            )
            |> Element.el [ Typography.caption, Font.color theme.textMuted ]
        ]
    , balanceText theme account.valuation Nothing
    , completenessBadge theme account.valuation.completeness
    , Surface.hairline theme
    , Typography.subsectionTitle theme "Balances included"
    , Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
        (List.map (detailBalance theme) account.subtreeBalances)
    , Element.text
        (let
            (Date.Date date) =
                account.valuation.asOf
         in
         "Valued as of " ++ TimeExtra.formatDate date
        )
        |> Element.el [ Typography.caption, Font.color theme.textMuted ]
    ]


detailForPath : Theme -> String -> Response -> Element Msg
detailForPath theme path response =
    case findAccount (Account.accountPath path) response.accounts of
        Just account ->
            accountDetailPage theme (Just BackToAccounts) account

        Nothing ->
            Page.scaffold theme
                { header = accountDetailHeader theme (Just BackToAccounts)
                , body =
                    [ Surface.card theme
                        [ Feedback.errorBanner theme
                            "That account is no longer present in the current snapshot."
                        ]
                    ]
                }


detailAccount : DetailState -> Maybe AccountNode
detailAccount detail =
    case detail of
        ShowingAccount account ->
            Just account

        _ ->
            Nothing


completenessBadge : Theme -> Completeness -> Element msg
completenessBadge theme completeness =
    case completeness of
        Completeness.Complete ->
            Chip.statusBadge theme Chip.StatusSuccess "Complete valuation"

        Completeness.Partial ->
            Chip.statusBadge theme Chip.StatusWarning "Partial valuation"

        Completeness.Unavailable ->
            Chip.statusBadge theme Chip.StatusDanger "Valuation unavailable"


detailBalance : Theme -> Quantity -> Element msg
detailBalance theme balance =
    Element.row
        [ Element.width Element.fill
        , Element.paddingXY Tokens.space8 Tokens.space12
        , Background.color theme.inputFill
        , Border.rounded Tokens.radiusControl
        , Element.spacing Tokens.space8
        ]
        [ Element.text (Quantity.commodity balance)
            |> Element.el
                [ Element.width Element.fill
                , Common.minWidthZero
                , Element.clip
                , Common.uiClass "ui-text-ellipsis"
                , Font.color theme.textMedium
                ]
        , Element.text (Formatting.formatBalance balance)
            |> Element.el [ Element.alignRight, Font.semiBold, Font.color theme.textPrimary ]
        ]


findAccount : AccountPath -> List AccountNode -> Maybe AccountNode
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
