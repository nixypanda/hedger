module LedgerRoutingTest exposing (suite)

import Expect
import LedgerRouting
import Route exposing (Route(..))
import Test exposing (Test, describe, test)
import Url exposing (Url)


suite : Test
suite =
    describe "ledger routing"
        [ test "redirects an old page path through the default ledger" <|
            \_ ->
                url "/cashflow"
                    |> Maybe.andThen (LedgerRouting.entryRedirect (Just "personal"))
                    |> Expect.equal (Just "/ledgers/personal/cashflow")
        , test "keeps the picker when there is no default" <|
            \_ ->
                url "/"
                    |> Maybe.andThen (LedgerRouting.entryRedirect Nothing)
                    |> Expect.equal Nothing
        , test "recognizes a scoped settings page" <|
            \_ ->
                url "/ledgers/business/settings"
                    |> Maybe.andThen LedgerRouting.scopedLocation
                    |> Expect.equal (Just ( "business", Settings ))
        , test "switching from Settings stays in Settings" <|
            \_ ->
                url "/ledgers/personal/settings"
                    |> Maybe.map (LedgerRouting.selectionRoute (Just "personal") Settings)
                    |> Expect.equal (Just Settings)
        , test "choosing from an unscoped path opens that page" <|
            \_ ->
                url "/transactions"
                    |> Maybe.map (LedgerRouting.selectionRoute Nothing NetWorth)
                    |> Expect.equal (Just Transactions)
        ]


url : String -> Maybe Url
url path =
    Url.fromString ("https://hedger.example" ++ path)
