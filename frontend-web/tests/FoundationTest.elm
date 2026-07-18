module FoundationTest exposing (suite)

import Api.Error as ApiError
import Api.Metadata as Metadata
import Api.Revision as Revision
import Data.Decimal as Decimal
import Expect
import Json.Decode as Decode
import Preferences exposing (ThemeMode(..))
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))
import Test exposing (Test, describe, test)
import Url


suite : Test
suite =
    describe "Phase 3 foundation"
        [ describe "decimal transport values"
            [ test "preserves precision as text" <|
                \_ ->
                    Decimal.fromString "-12345678901234567890.004200"
                        |> Result.map Decimal.toString
                        |> Expect.equal (Ok "-12345678901234567890.004200")
            , test "rejects exponent notation" <|
                \_ ->
                    Decimal.fromString "1e6"
                        |> Result.map Decimal.toString
                        |> Expect.err
            ]
        , test "root and visible routes parse" <|
            \_ ->
                [ "/"
                , "/net-worth"
                , "/investments"
                , "/cashflow"
                , "/accounts"
                , "/accounts/Assets%3ACash"
                , "/transactions"
                , "/settings"
                ]
                    |> List.filterMap (\path -> Url.fromString ("https://hedger.test" ++ path))
                    |> List.map Route.fromUrl
                    |> Expect.equal
                        [ NetWorth
                        , NetWorth
                        , Investments
                        , Cashflow
                        , Accounts
                        , Account "Assets:Cash"
                        , Transactions
                        , Settings
                        ]
        , test "loading preserves stale content" <|
            \_ ->
                Success 42
                    |> RemoteData.beginLoading
                    |> RemoteData.value
                    |> Expect.equal (Just 42)
        , test "legacy theme preference migrates to version one" <|
            \_ ->
                Decode.decodeString Preferences.decoder "{\"themeMode\":\"dark\"}"
                    |> Result.map (\preferences -> ( preferences.version, preferences.themeMode ))
                    |> Expect.equal (Ok ( 1, Dark ))
        , test "revision mismatches are explicit" <|
            \_ ->
                Revision.ensureMatching (Just "revision-a")
                    { revision = Just "revision-b"
                    , freshness = Metadata.Fresh
                    , warnings = []
                    }
                    |> Expect.equal
                        (Err { expected = "revision-a", received = "revision-b" })
        , test "stable API error envelope decodes" <|
            \_ ->
                Decode.decodeString ApiError.decoder
                    """
                    {"errorPayload":{"code":"journal_unavailable",
                    "message":"No snapshot",
                    "requestId":"req-1",
                    "details":null}}
                    """
                    |> Result.map .code
                    |> Expect.equal (Ok "journal_unavailable")
        ]
