module Page exposing (Context, Model(..), Msg(..), OutMsg(..), init, update, view)

import Element exposing (Element)
import Page.Account as AccountPage
import Page.AccountRegister as AccountRegisterPage
import Page.Cashflow as CashflowPage
import Page.NetWorth as NetWorthPage
import Page.Transactions as TransactionsPage
import Route exposing (Route(..))
import State.Viewport exposing (Viewport)
import Ui.Theme exposing (Theme)



-- MODEL


type Model
    = AccountsPage AccountPage.Model
    | TransactionsPage TransactionsPage.Model
    | AccountRegisterPage AccountRegisterPage.Model
    | NetWorthPage NetWorthPage.Model
    | CashflowPage CashflowPage.Model
    | PlaceholderPage


type alias Context =
    { route : Route
    , viewport : Viewport
    , theme : Theme
    }


mapAccountOutMsg : AccountPage.OutMsg -> OutMsg
mapAccountOutMsg outMsg =
    case outMsg of
        AccountPage.NoOutMsg ->
            NoOutMsg

        AccountPage.Navigate route ->
            Navigate route


init : Context -> Maybe Model -> ( Model, Cmd Msg, OutMsg )
init context maybeExistingModel =
    case ( context.route, maybeExistingModel ) of
        ( Transactions, Just (TransactionsPage existingModel) ) ->
            let
                ( nextModel, request ) =
                    TransactionsPage.beginLoading existingModel
            in
            ( TransactionsPage nextModel
            , Cmd.map GotTransactionsMsg (TransactionsPage.fetch request)
            , NoOutMsg
            )

        ( NetWorth, _ ) ->
            let
                ( model, command ) =
                    NetWorthPage.init
            in
            ( NetWorthPage model, Cmd.map GotNetWorthMsg command, NoOutMsg )

        ( Cashflow, _ ) ->
            let
                ( model, command ) =
                    CashflowPage.init
            in
            ( CashflowPage model, Cmd.map GotCashflowMsg command, NoOutMsg )

        ( Transactions, _ ) ->
            let
                ( nextModel, request ) =
                    TransactionsPage.beginLoading TransactionsPage.init
            in
            ( TransactionsPage nextModel
            , Cmd.map GotTransactionsMsg (TransactionsPage.fetch request)
            , NoOutMsg
            )

        ( Accounts, Just (AccountsPage existingModel) ) ->
            ( AccountsPage (AccountPage.beginLoading existingModel)
            , Cmd.map GotAccountsMsg AccountPage.fetchAccounts
            , NoOutMsg
            )

        ( Accounts, _ ) ->
            ( AccountsPage (AccountPage.beginLoading AccountPage.init)
            , Cmd.map GotAccountsMsg AccountPage.fetchAccounts
            , NoOutMsg
            )

        ( Account accountName, Just (AccountRegisterPage existingModel) ) ->
            let
                ( nextModel, request ) =
                    AccountRegisterPage.beginLoading accountName existingModel
            in
            ( AccountRegisterPage nextModel
            , Cmd.map GotAccountRegisterMsg (AccountRegisterPage.fetch request)
            , NoOutMsg
            )

        ( Account accountName, _ ) ->
            let
                ( nextModel, request ) =
                    AccountRegisterPage.beginLoading accountName AccountRegisterPage.init
            in
            ( AccountRegisterPage nextModel
            , Cmd.map GotAccountRegisterMsg (AccountRegisterPage.fetch request)
            , NoOutMsg
            )

        ( AccountValue _, Just (AccountsPage existingModel) ) ->
            ( AccountsPage (AccountPage.beginLoading existingModel)
            , Cmd.map GotAccountsMsg AccountPage.fetchAccounts
            , NoOutMsg
            )

        ( AccountValue accountName, _ ) ->
            ( AccountsPage (AccountPage.beginLoading (AccountPage.initWithValueSelected accountName))
            , Cmd.map GotAccountsMsg AccountPage.fetchAccounts
            , NoOutMsg
            )

        _ ->
            ( PlaceholderPage, Cmd.none, NoOutMsg )



-- UPDATE


type Msg
    = GotAccountsMsg AccountPage.Msg
    | GotTransactionsMsg TransactionsPage.Msg
    | GotAccountRegisterMsg AccountRegisterPage.Msg
    | GotNetWorthMsg NetWorthPage.Msg
    | GotCashflowMsg CashflowPage.Msg


type OutMsg
    = NoOutMsg
    | Navigate Route


mapTransactionsOutMsg : TransactionsPage.OutMsg -> OutMsg
mapTransactionsOutMsg outMsg =
    case outMsg of
        TransactionsPage.NoOutMsg ->
            NoOutMsg


mapAccountRegisterOutMsg : AccountRegisterPage.OutMsg -> OutMsg
mapAccountRegisterOutMsg outMsg =
    case outMsg of
        AccountRegisterPage.NoOutMsg ->
            NoOutMsg

        AccountRegisterPage.Navigate route ->
            Navigate route


update : Context -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update context msg model =
    case ( msg, model ) of
        ( GotAccountsMsg subMsg, AccountsPage subModel ) ->
            let
                ( nextModel, subCmd, outMsg ) =
                    AccountPage.update { viewport = context.viewport } subMsg subModel
            in
            ( AccountsPage nextModel
            , Cmd.map GotAccountsMsg subCmd
            , mapAccountOutMsg outMsg
            )

        ( GotTransactionsMsg subMsg, TransactionsPage subModel ) ->
            let
                ( nextModel, subCmd, outMsg ) =
                    TransactionsPage.update subMsg subModel
            in
            ( TransactionsPage nextModel
            , Cmd.map GotTransactionsMsg subCmd
            , mapTransactionsOutMsg outMsg
            )

        ( GotAccountRegisterMsg subMsg, AccountRegisterPage subModel ) ->
            let
                ( nextModel, subCmd, outMsg ) =
                    AccountRegisterPage.update subMsg subModel
            in
            ( AccountRegisterPage nextModel
            , Cmd.map GotAccountRegisterMsg subCmd
            , mapAccountRegisterOutMsg outMsg
            )

        ( GotNetWorthMsg subMsg, NetWorthPage subModel ) ->
            let
                ( nextModel, command ) =
                    NetWorthPage.update subMsg subModel
            in
            ( NetWorthPage nextModel, Cmd.map GotNetWorthMsg command, NoOutMsg )

        ( GotCashflowMsg subMsg, CashflowPage subModel ) ->
            let
                ( nextModel, command ) =
                    CashflowPage.update subMsg subModel
            in
            ( CashflowPage nextModel, Cmd.map GotCashflowMsg command, NoOutMsg )

        _ ->
            ( model, Cmd.none, NoOutMsg )



-- VIEW


view : Context -> Model -> Element Msg
view context model =
    let
        device =
            Element.classifyDevice context.viewport
    in
    case ( context.route, model ) of
        ( AccountValue accountName, AccountsPage subModel ) ->
            Element.map GotAccountsMsg
                (AccountPage.valueView context.theme device accountName subModel)

        ( _, AccountsPage subModel ) ->
            Element.map GotAccountsMsg
                (AccountPage.view context.theme context.viewport subModel)

        ( _, TransactionsPage subModel ) ->
            Element.map GotTransactionsMsg
                (TransactionsPage.view context.theme context.viewport subModel)

        ( _, AccountRegisterPage subModel ) ->
            Element.map GotAccountRegisterMsg
                (AccountRegisterPage.view context.theme context.viewport subModel)

        ( _, NetWorthPage subModel ) ->
            Element.map GotNetWorthMsg (NetWorthPage.view context.theme context.viewport subModel)

        ( _, CashflowPage subModel ) ->
            Element.map GotCashflowMsg (CashflowPage.view context.theme context.viewport subModel)

        _ ->
            Element.none
