module FoundationTest exposing (suite)

import Data.Decimal as Decimal
import Data.Revision as Revision
import Expect
import Fuzz
import Json.Decode as Decode
import Preferences
import RemoteData exposing (RemoteData(..))
import Route exposing (Route(..))
import Test exposing (Test, describe, fuzz2, test)
import Ui.Theme exposing (Mode(..))
import Ui.Viewport as Viewport exposing (Viewport)
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
                    |> Result.map (\preferences -> ( preferences.version, preferences.mode ))
                    |> Expect.equal (Ok ( 1, Ui.Theme.Dark ))
        , describe "theme mode decoding"
            [ test "accepts light" <|
                \_ ->
                    decodeMode "light"
                        |> Expect.equal (Ok Light)
            , test "accepts dark" <|
                \_ ->
                    decodeMode "dark"
                        |> Expect.equal (Ok Dark)
            , test "rejects unknown values" <|
                \_ ->
                    decodeMode "sepia"
                        |> Expect.err
            ]
        , fuzz2 (Fuzz.maybe Fuzz.string)
            (Fuzz.maybe Fuzz.string)
            "snapshots are compatible only when revisions match or are absent"
            revisionCompatibility
        , test "responsive layouts use available width efficiently" <|
            responsiveLayoutsUseWidth
        , test "content layout switches at usable-width boundaries" <|
            contentLayoutBoundaries
        , test "usable content width matches layout boundaries" <|
            usableContentWidthBoundaries
        , test "master/detail starts at wide layouts" <|
            masterDetailBoundaries
        , test "viewport height does not change structural layout" <|
            responsiveLayoutIgnoresHeight
        , test "full navigation does not shrink usable content" <|
            navigationTransitionPreservesContentWidth
        ]


viewportAt : Int -> Viewport
viewportAt width =
    { width = width, height = 900 }


decodeMode : String -> Result Decode.Error Mode
decodeMode value =
    Decode.decodeString Preferences.decoder ("{\"themeMode\":\"" ++ value ++ "\"}")
        |> Result.map .mode


contentLayoutBoundaries : () -> Expect.Expectation
contentLayoutBoundaries _ =
    [ 731, 732, 991, 992 ]
        |> List.map (viewportAt >> Viewport.contentLayout)
        |> Expect.equal
            [ Viewport.Compact
            , Viewport.Medium
            , Viewport.Medium
            , Viewport.Wide
            ]


usableContentWidthBoundaries : () -> Expect.Expectation
usableContentWidthBoundaries _ =
    [ 732, 992 ]
        |> List.map (viewportAt >> Viewport.usableContentWidth)
        |> Expect.equal [ 700, 960 ]


masterDetailBoundaries : () -> Expect.Expectation
masterDetailBoundaries _ =
    [ 991, 992 ]
        |> List.map (viewportAt >> Viewport.usesMasterDetail)
        |> Expect.equal [ False, True ]


responsiveLayoutsUseWidth : () -> Expect.Expectation
responsiveLayoutsUseWidth _ =
    [ { width = 390, height = 844 }
    , { width = 768, height = 1024 }
    , { width = 1024, height = 768 }
    ]
        |> List.map Viewport.contentLayout
        |> Expect.equal
            [ Viewport.Compact
            , Viewport.Medium
            , Viewport.Wide
            ]


responsiveLayoutIgnoresHeight : () -> Expect.Expectation
responsiveLayoutIgnoresHeight _ =
    [ { width = 768, height = 500 }
    , { width = 768, height = 1366 }
    ]
        |> List.map
            (\viewport ->
                ( Viewport.contentLayout viewport
                , Viewport.usesMasterDetail viewport
                )
            )
        |> Expect.equal
            [ ( Viewport.Medium, False )
            , ( Viewport.Medium, False )
            ]


navigationTransitionPreservesContentWidth : () -> Expect.Expectation
navigationTransitionPreservesContentWidth _ =
    let
        before =
            viewportAt 1543

        after =
            viewportAt 1544
    in
    { beforeUsesBottomNavigation = Viewport.usesBottomNavigation before
    , afterUsesBottomNavigation = Viewport.usesBottomNavigation after
    , beforeContentWidth = Viewport.usableContentWidth before
    , afterContentWidth = Viewport.usableContentWidth after
    }
        |> Expect.equal
            { beforeUsesBottomNavigation = True
            , afterUsesBottomNavigation = False
            , beforeContentWidth = 1280
            , afterContentWidth = 1280
            }


{-| Two snapshots are compatible only when they name the same journal
revision, or when either side has not yet observed a revision.
-}
revisionCompatibility : Maybe String -> Maybe String -> Expect.Expectation
revisionCompatibility expected received =
    case ( expected, received ) of
        ( Just expectedRevision, Just receivedRevision ) ->
            if expectedRevision == receivedRevision then
                Revision.ensureMatching expected received
                    |> Expect.equal (Ok ())

            else
                Revision.ensureMatching expected received
                    |> Expect.equal
                        (Err { expected = expectedRevision, received = receivedRevision })

        _ ->
            Revision.ensureMatching expected received
                |> Expect.equal (Ok ())
