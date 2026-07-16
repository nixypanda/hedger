module Route exposing (Route(..), fromUrl, parser, toPath)

import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, s, top)


type Route
    = NetWorth
    | Investments
    | Cashflow
    | Accounts
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
        , map Accounts (s "accounts")
        , map Transactions (s "transactions")
        , map Settings (s "settings")
        , map Review (s "review")
        ]


fromUrl : Url -> Route
fromUrl url =
    Url.Parser.parse parser url
        |> Maybe.withDefault NetWorth


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

        Transactions ->
            "/transactions"

        Settings ->
            "/settings"

        Review ->
            "/review"
