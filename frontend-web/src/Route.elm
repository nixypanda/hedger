module Route exposing (Route(..), fromUrl, parser, toPath)

import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, s, string, top)


type Route
    = NetWorth
    | Investments
    | Cashflow
    | Accounts
    | Account String
    | AccountValue String
    | Transactions
    | Settings
    | Review


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map NetWorth top
        , map NetWorth (s "net-worth")
        , map Investments (s "investments")
        , map Cashflow (s "cashflow")
        , map AccountValue (s "accounts" </> string </> s "value")
        , map Account (s "accounts" </> string)
        , map Accounts (s "accounts")
        , map Transactions (s "transactions")
        , map Settings (s "settings")
        , map Review (s "review")
        ]


fromUrl : Url -> Route
fromUrl url =
    Url.Parser.parse parser url
        |> Maybe.map decodeAccountRoute
        |> Maybe.withDefault NetWorth


decodeAccountRoute : Route -> Route
decodeAccountRoute route =
    case route of
        Account accountName ->
            Account (Url.percentDecode accountName |> Maybe.withDefault accountName)

        AccountValue accountName ->
            AccountValue (Url.percentDecode accountName |> Maybe.withDefault accountName)

        _ ->
            route


toPath : Route -> String
toPath route =
    case route of
        NetWorth ->
            "/net-worth"

        Investments ->
            "/investments"

        Cashflow ->
            "/cashflow"

        Accounts ->
            "/accounts"

        Account accountName ->
            "/accounts/" ++ Url.percentEncode accountName

        AccountValue accountName ->
            "/accounts/" ++ Url.percentEncode accountName ++ "/value"

        Transactions ->
            "/transactions"

        Settings ->
            "/settings"

        Review ->
            "/review"
