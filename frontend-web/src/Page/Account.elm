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
import Data.Freshness exposing (Freshness(..))
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
import Ui.Notice as Notice
import Ui.Page as Page
import Ui.Skeleton as Skeleton
import Ui.Surface as Surface
import Ui.Theme exposing (Theme)
import Ui.Typography as Typography
import Ui.Viewport as Viewport exposing (Viewport)
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
    = Refresh
    | ToggleAccount AccountPath
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
        Refresh ->
            ( beginLoading (Model model), fetchAccounts, NoOutMsg )

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
            Page.scaffold viewport
                theme
                { header =
                    { leading = Nothing
                    , title = Typography.pageTitle theme "Accounts"
                    , subtitle = Nothing
                    , actions =
                        [ States.refreshStatus theme
                            { freshness = accountsFreshness model.accounts
                            , loading = accountsLoading model.accounts
                            }
                        , Button.refreshButton theme
                            { loading = accountsLoading model.accounts
                            , onPress = Refresh
                            }
                        ]
                    }
                , body =
                    content theme model
                        :: failureNotices theme model.accounts
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
                      , content =
                            accountDetailPage viewport theme (Just BackToAccounts) account
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
            accountsSkeleton theme

        Loading ->
            accountsSkeleton theme

        Refreshing response ->
            accountCard theme model.expanded response

        Loaded response ->
            accountCard theme model.expanded response

        FailedWithStale _ response ->
            accountCard theme model.expanded response

        Failed error ->
            Surface.card theme
                [ Feedback.errorBanner theme
                    ("Accounts failed to load: " ++ ApiError.message error)
                ]


accountCard : Theme -> Set String -> Response -> Element Msg
accountCard theme expanded response =
    if List.isEmpty response.accounts then
        accountsEmptyCard theme

    else
        Surface.card theme
            [ accountTree theme expanded response.accounts ]


accountTree :
    Theme
    -> Set String
    -> List AccountNode
    -> Element Msg
accountTree theme expanded accounts =
    Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
        (List.map (accountRow theme expanded 0) accounts)


accountsEmptyCard : Theme -> Element msg
accountsEmptyCard theme =
    Surface.cardWith
        [ Element.height
            (Element.minimum accountsCardHeight Element.shrink)
        ]
        theme
        [ Typography.mutedParagraph theme "No accounts were found in the journal." ]


failureNotices : Theme -> AccountsState -> List (Element Msg)
failureNotices theme accounts =
    case accounts of
        FailedWithStale error _ ->
            [ Notice.view theme
                { placement = Notice.Bottom
                , tone = Notice.Danger
                , icon = Icon.AlertTriangle
                , message =
                    "Refresh failed; showing stale accounts: " ++ ApiError.message error
                , actions = []
                , id = Just "accounts-refresh-error"
                }
            ]

        _ ->
            []


accountsLoading : AccountsState -> Bool
accountsLoading accounts =
    case accounts of
        Loading ->
            True

        Refreshing _ ->
            True

        _ ->
            False


accountsFreshness : AccountsState -> Freshness
accountsFreshness accounts =
    case accounts of
        Loaded response ->
            response.metadata.freshness

        Refreshing response ->
            response.metadata.freshness

        FailedWithStale _ _ ->
            Stale

        _ ->
            Unavailable


accountRowHeight : Int
accountRowHeight =
    Tokens.minimumTouchTarget
        + 2
        * Tokens.space8
        + 2
        * Tokens.borderHairline


accountsListContentHeight : Int
accountsListContentHeight =
    5
        * accountRowHeight
        + 4
        * Tokens.space8


accountsCardHeight : Int
accountsCardHeight =
    accountsListContentHeight
        + 2
        * Tokens.space20
        + 2
        * Tokens.borderHairline


accountsSkeleton : Theme -> Element msg
accountsSkeleton theme =
    Surface.card theme
        [ Element.column
            [ Element.width Element.fill, Element.spacing Tokens.space8 ]
            (List.repeat 5 (accountRowSkeleton theme))
        ]


accountRowSkeleton : Theme -> Element msg
accountRowSkeleton theme =
    Element.row
        (Surface.rowSurfaceAttrs theme False
            ++ [ Element.paddingXY Tokens.space12 Tokens.space8
               , Element.spacing Tokens.space4
               ]
        )
        [ Element.el
            [ Element.width (Element.px Tokens.minimumTouchTarget) ]
            Element.none
        , Element.row
            [ Element.width (Element.fillPortion Tokens.layoutPortionTriple)
            , Common.touchTargetHeight
            , Element.spacing Tokens.space8
            ]
            [ Skeleton.block
                theme
                Tokens.iconSizeCaption
                Tokens.iconSizeCaption
            , Element.column
                [ Element.width Element.fill, Element.spacing Tokens.space2 ]
                [ Skeleton.block theme Tokens.skeletonLineLong Tokens.bodyTextHeight
                , Skeleton.block theme Tokens.skeletonLineShort Tokens.space12
                ]
            ]
        , Element.el
            [ Element.width (Element.fillPortion Tokens.layoutPortionSingle)
            , Element.alignRight
            ]
            (Element.column
                [ Element.width Element.fill
                , Element.spacing Tokens.space2
                ]
                [ Element.el [ Element.alignRight ]
                    (Skeleton.block theme Tokens.skeletonLineShort Tokens.bodyTextHeight)
                , Element.el [ Element.alignRight ]
                    (Skeleton.block theme Tokens.skeletonLineTiny Tokens.space12)
                ]
            )
        ]


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
                            [ Common.truncatedText account.name
                            , Element.text
                                (account.accountType
                                    |> Maybe.map AccountType.toString
                                    |> Maybe.withDefault "account"
                                )
                                |> Element.el (Typography.caption ++ [ Font.color theme.textMuted ])
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
            (Typography.body
                ++ [ Element.alignRight
                   , Font.color theme.textPrimary
                   , Typography.fontWeight Typography.SemiBold
                   , Common.minWidthZero
                   , Common.uiClass "ui-text-ellipsis"
                   ]
            )
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
                    (Typography.caption
                        ++ [ Element.alignRight
                           , Font.color theme.textMuted
                           ]
                    )
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
            detailForPath env.viewport theme path response

        Refreshing response ->
            detailForPath env.viewport theme path response

        FailedWithStale _ response ->
            detailForPath env.viewport theme path response

        _ ->
            Page.scaffold env.viewport
                theme
                { header = accountDetailHeader theme (Just BackToAccounts)
                , body =
                    [ Surface.card theme [ Feedback.loading theme "Loading account value…" ] ]
                }


accountDetailPage : Viewport -> Theme -> Maybe Msg -> AccountNode -> Element Msg
accountDetailPage viewport theme onBack account =
    Page.scaffold viewport
        theme
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
            |> Element.el (Typography.caption ++ [ Font.color theme.textMuted ])
        ]
    , balanceText theme account.valuation Nothing
    , completenessBadge theme account.valuation.completeness
    , Surface.hairline theme
    , Typography.cardTitle theme "Balances included"
    , Element.column [ Element.width Element.fill, Element.spacing Tokens.space8 ]
        (List.map (detailBalance theme) account.subtreeBalances)
    , Element.text
        (let
            (Date.Date date) =
                account.valuation.asOf
         in
         "Valued as of " ++ TimeExtra.formatDate date
        )
        |> Element.el (Typography.caption ++ [ Font.color theme.textMuted ])
    ]


detailForPath : Viewport -> Theme -> String -> Response -> Element Msg
detailForPath viewport theme path response =
    case findAccount (Account.accountPath path) response.accounts of
        Just account ->
            accountDetailPage viewport theme (Just BackToAccounts) account

        Nothing ->
            Page.scaffold viewport
                theme
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
            |> Element.el
                (Typography.body
                    ++ [ Element.alignRight
                       , Typography.fontWeight Typography.SemiBold
                       , Font.color theme.textPrimary
                       ]
                )
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
