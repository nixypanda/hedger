module FoundationTest exposing (suite)

import Api.Error as ApiError
import Data.Decimal as Decimal
import Data.Revision as Revision
import Expect
import Json.Decode as Decode
import Preferences exposing (ThemeMode(..))
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))
import Test exposing (Test, describe, test)
import Ui.Responsive as Responsive
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
                    (Just "revision-b")
                    |> Expect.equal
                        (Err { expected = "revision-a", received = "revision-b" })
        , test "equal or absent revisions are compatible" <|
            \_ ->
                [ Revision.ensureMatching (Just "revision-a") (Just "revision-a")
                , Revision.ensureMatching Nothing (Just "revision-a")
                , Revision.ensureMatching (Just "revision-a") Nothing
                ]
                    |> Expect.equal [ Ok (), Ok (), Ok () ]
        , test "stable API error envelope decodes" <|
            \_ ->
                Decode.decodeString ApiError.decoder
                    """
                    {"error":{"code":"journal_unavailable",
                    "message":"No snapshot",
                    "requestId":"req-1",
                    "details":null}}
                    """
                    |> Result.map .code
                    |> Expect.equal (Ok "journal_unavailable")
        , test "responsive layouts use available width efficiently" <|
            responsiveLayoutsUseWidth
        , test "viewport height does not change structural layout" <|
            responsiveLayoutIgnoresHeight
        , test "full navigation does not shrink usable content" <|
            navigationTransitionPreservesContentWidth
        ]


responsiveLayoutsUseWidth : () -> Expect.Expectation
responsiveLayoutsUseWidth _ =
    [ { width = 390, height = 844 }
    , { width = 768, height = 1024 }
    , { width = 1024, height = 768 }
    ]
        |> List.map Responsive.contentLayout
        |> Expect.equal
            [ Responsive.Compact
            , Responsive.Medium
            , Responsive.Wide
            ]


responsiveLayoutIgnoresHeight : () -> Expect.Expectation
responsiveLayoutIgnoresHeight _ =
    [ { width = 768, height = 500 }
    , { width = 768, height = 1366 }
    ]
        |> List.map
            (\viewport ->
                ( Responsive.contentLayout viewport
                , Responsive.usesMasterDetail viewport
                )
            )
        |> Expect.equal
            [ ( Responsive.Medium, False )
            , ( Responsive.Medium, False )
            ]


navigationTransitionPreservesContentWidth : () -> Expect.Expectation
navigationTransitionPreservesContentWidth _ =
    let
        before =
            { width = 1543, height = 900 }

        after =
            { width = 1544, height = 900 }
    in
    { beforeUsesBottomNavigation = Responsive.usesBottomNavigation before
    , afterUsesBottomNavigation = Responsive.usesBottomNavigation after
    , beforeContentWidth = Responsive.usableContentWidth before
    , afterContentWidth = Responsive.usableContentWidth after
    }
        |> Expect.equal
            { beforeUsesBottomNavigation = True
            , afterUsesBottomNavigation = False
            , beforeContentWidth = 1280
            , afterContentWidth = 1280
            }
