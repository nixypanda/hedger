module Hedger.Interface.HTTP.APISpec (apiSpec) where

import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text.Encoding qualified as TextEncoding
import Data.Vector qualified as Vector
import Network.Wai qualified as Wai
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Hedger.Runtime.Server (makeApplication)
import Hedger.TestHelpers (configuration, getJson, loadedStore)

apiSpec :: TestTree
apiSpec = withResource setup teardown $ \getApp ->
    testGroup
        "API"
        [ testCase "fetches accounts tree and register" $ do
            app <- getApp
            accountsAndRegister app
        , testCase "fetches transactions with sequential cursor" $ do
            app <- getApp
            transactionsCursor app
        ]

setup :: IO Wai.Application
setup = do
    store <- loadedStore
    pure (makeApplication configuration store)

teardown :: Wai.Application -> IO ()
teardown = const (pure ())

accountsAndRegister :: Wai.Application -> IO ()
accountsAndRegister app = do
    (Just tree, _) <- getJson "/api/v1/accounts/tree" app
    case tree of
        Object obj -> case KeyMap.lookup "accounts" obj of
            Just (Array accounts) ->
                assertEqual "account tree should have 5 top-level accounts" 5 (Vector.length accounts)
            _ -> assertFailure "expected accounts array"
        _ -> assertFailure "expected object"
    (Just reg, _) <- getJson "/api/v1/accounts/assets:cash:checking/register?limit=2" app
    case reg of
        Object obj -> case KeyMap.lookup "entries" obj of
            Just (Array entries) ->
                assertEqual "register should return 2 entries" 2 (Vector.length entries)
            _ -> assertFailure "expected entries array"
        _ -> assertFailure "expected object"

transactionsCursor :: Wai.Application -> IO ()
transactionsCursor app = do
    (Just first, _) <- getJson "/api/v1/transactions?limit=5" app
    cursor <- case first of
        Object obj -> case KeyMap.lookup "nextCursor" obj of
            Just (String c) -> pure c
            _ -> assertFailure "expected nextCursor string" >> pure ""
        _ -> assertFailure "expected object" >> pure ""
    (Just second, _) <- getJson ("/api/v1/transactions?limit=5&cursor=" <> TextEncoding.encodeUtf8 cursor) app
    case second of
        Object obj -> case KeyMap.lookup "transactions" obj of
            Just (Array txns) -> do
                assertEqual "second page should have 5 transactions" 5 (Vector.length txns)
                case (first, Vector.head txns) of
                    (Object firstObj, Object firstTxn) ->
                        case (KeyMap.lookup "transactions" firstObj, KeyMap.lookup "description" firstTxn) of
                            (Just (Array firstPage), Just (String desc1)) -> case Vector.head firstPage of
                                Object firstPageFirst -> case KeyMap.lookup "description" firstPageFirst of
                                    Just (String desc2) ->
                                        assertBool
                                            "pages should contain different transactions"
                                            (desc1 /= desc2)
                                    _ -> assertFailure "expected description string"
                                _ -> assertFailure "expected object"
                            _ -> assertFailure "expected transactions array with description"
                    _ -> assertFailure "expected objects"
            _ -> assertFailure "expected transactions array"
        _ -> assertFailure "expected object"
