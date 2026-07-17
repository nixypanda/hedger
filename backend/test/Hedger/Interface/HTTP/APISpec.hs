module Hedger.Interface.HTTP.APISpec (apiSpec) where

import Data.Aeson (decode, eitherDecode)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Either (isLeft)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Hedger.Application.TransactionQuery (maximumQueryLength)
import Hedger.Infrastructure.SnapshotStore (
    newSnapshotStore,
    reloadSnapshot,
 )
import Hedger.Interface.HTTP.Types (
    Bootstrap (..),
    ResponseMetadata (..),
    TransactionDto (..),
    TransactionsResponse (..),
 )
import Hedger.Runtime.Server (makeApplication)
import Hedger.TestHelpers (
    assertQueryCount,
    configuration,
    decodeTransactions,
    errorCode,
    get,
    getQuery,
    getWithRequestId,
    json,
    loadedStore,
 )
import Network.HTTP.Types (hContentType, status200, status400, status404, status503)
import Network.Wai qualified as Wai
import Network.Wai.Test (SResponse (..))
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

apiSpec :: TestTree
apiSpec = withResource setup teardown $ \getApps ->
    testGroup
        "API"
        [ testCase "serves live, ready, and bootstrap from the fixture" $ do
            (availableApp, _) <- getApps
            servesHealthAndBootstrap availableApp
        , testCase "serves the complete transaction shape and shared response metadata" $ do
            (availableApp, _) <- getApps
            servesTransactionShape availableApp
        , testCase "applies hledger account, description, code, date, status, amount, tag queries" $ do
            (availableApp, _) <- getApps
            appliesHledgerQueries availableApp
        , testCase "returns stable safe parser details without echoing the query" $ do
            (availableApp, _) <- getApps
            returnsSafeParserDetails availableApp
        , testCase "rejects report options, invalid limits, and overlong queries with stable codes" $ do
            (availableApp, _) <- getApps
            rejectsInvalidQueries availableApp
        , testCase "returns opaque cursor pages and rejects query-conflicting cursors" $ do
            (availableApp, _) <- getApps
            returnsCursorPages availableApp
        , testCase "marks retained transaction data stale after a failed reload" marksStaleAfterFailedReload
        , testCase "keeps liveness up but reports an unavailable snapshot" $ do
            (_, unavailableApp) <- getApps
            reportsUnavailableSnapshot unavailableApp
        , testCase "returns stable JSON for unknown routes" $ do
            (availableApp, _) <- getApps
            returnsStableNotFound availableApp
        ]

setup :: IO (Wai.Application, Wai.Application)
setup = do
    availableStore <- loadedStore
    unavailableStore <- newSnapshotStore
    let availableApp = makeApplication configuration availableStore
        unavailableApp = makeApplication configuration unavailableStore
    pure (availableApp, unavailableApp)

teardown :: (Wai.Application, Wai.Application) -> IO ()
teardown = const (pure ())

servesHealthAndBootstrap :: Wai.Application -> IO ()
servesHealthAndBootstrap availableApp = do
    live <- get "/api/v1/health/live" availableApp
    assertEqual "" status200 (simpleStatus live)
    assertBool
        "X-Request-ID should start with req-"
        (maybe False (ByteString.isPrefixOf "req-") (lookup "X-Request-ID" (simpleHeaders live)))
    assertEqual "" (decode "{\"status\":\"ok\"}") (json live)
    ready <- get "/api/v1/health/ready" availableApp
    assertEqual "" status200 (simpleStatus ready)
    bootstrap <- get "/api/v1/bootstrap" availableApp
    assertEqual "" status200 (simpleStatus bootstrap)
    case eitherDecode bootstrap.simpleBody :: Either String Bootstrap of
        Left decodeError -> assertFailure decodeError
        Right value -> do
            assertEqual "" "v1" value.apiVersion
            assertEqual "" "fresh" value.metadata.freshness
            assertBool "revision should start with md5:" (Text.isPrefixOf "md5:" value.metadata.revision)

servesTransactionShape :: Wai.Application -> IO ()
servesTransactionShape availableApp = do
    response <- get "/api/v1/transactions" availableApp
    assertEqual "" status200 (simpleStatus response)
    case eitherDecode response.simpleBody :: Either String TransactionsResponse of
        Left decodeError -> assertFailure decodeError
        Right value -> do
            assertEqual "" "fresh" value.metadata.freshness
            assertBool "revision should start with md5:" (Text.isPrefixOf "md5:" value.metadata.revision)
            assertEqual "" 13 (length value.transactions)
            assertEqual "" 27 (sum (length . (.postings) <$> value.transactions))
            assertBool
                "all transactions should have non-empty descriptions"
                (all (not . Text.null . (.description)) value.transactions)
            assertEqual "" Nothing value.nextCursor
    emptyQuery <- getQuery "?query=" availableApp >>= decodeTransactions
    assertEqual "" 13 (length emptyQuery.transactions)

appliesHledgerQueries :: Wai.Application -> IO ()
appliesHledgerQueries availableApp = do
    let cases =
            [ ("acct%3Aexpenses", 4)
            , ("desc%3ASalary", 2)
            , ("code%3AUTIL", 1)
            , ("date%3A2024", 2)
            , ("date2%3A2024-04-03", 1)
            , ("status%3A%2A", 1)
            , ("cur%3AAAPL", 1)
            , ("amt%3A%3E100000", 1)
            , ("tag%3Acategory%3Dhousehold", 1)
            , ("not%3Adesc%3ASalary", 11)
            , ("expr%3A%27%28desc%3ASalary%20OR%20code%3AUTIL%29%27", 3)
            , ("any%3Aacct%3Aexpenses", 4)
            , ("all%3Aacct%3Aassets", 3)
            ]
    mapM_ (assertQueryCount availableApp) cases

returnsSafeParserDetails :: Wai.Application -> IO ()
returnsSafeParserDetails availableApp = do
    response <- getQuery "?query=expr%3A%27%28acct%3Aprivate-secret%20OR%20desc%3ARent%27" availableApp
    assertEqual "" status400 (simpleStatus response)
    assertBool
        "should not echo private-secret"
        (not . ByteString.Char8.isInfixOf "private-secret" $ LazyByteString.toStrict response.simpleBody)
    expected <- LazyByteString.readFile "test/golden/error-invalid-query.json"
    assertEqual "" (decode expected) (json response)

rejectsInvalidQueries :: Wai.Application -> IO ()
rejectsInvalidQueries availableApp = do
    unsupported <- getQuery "?query=inacct%3Aassets" availableApp
    assertEqual "" (Just "unsupported_query_option") (errorCode unsupported)
    expected <- LazyByteString.readFile "test/golden/error-unsupported-query-option.json"
    assertEqual "" (decode expected) (json unsupported)
    mapM_
        (\qs -> getQuery qs availableApp >>= \r -> assertEqual "" (Just "invalid_limit") (errorCode r))
        ["?limit=0", "?limit=201", "?limit=not-a-number"]
    let longQuery = "?query=" <> ByteString.replicate (maximumQueryLength + 1) 97
    overlong <- getQuery longQuery availableApp
    assertEqual "" (Just "query_too_long") (errorCode overlong)

returnsCursorPages :: Wai.Application -> IO ()
returnsCursorPages availableApp = do
    firstResponse <- getQuery "?query=acct%3Aassets&limit=2" availableApp
    first <- decodeTransactions firstResponse
    assertEqual "" 2 (length first.transactions)
    cursor <- case first.nextCursor of
        Nothing -> assertFailure "Expected a continuation cursor." >> pure ""
        Just c -> pure c
    secondResponse <-
        getQuery ("?query=acct%3Aassets&limit=2&cursor=" <> TextEncoding.encodeUtf8 cursor) availableApp
    second <- decodeTransactions secondResponse
    assertEqual "" 2 (length second.transactions)
    assertBool
        "transactions should be different"
        (fmap (.description) first.transactions /= fmap (.description) second.transactions)
    conflict <-
        getQuery ("?query=desc%3ASalary&limit=2&cursor=" <> TextEncoding.encodeUtf8 cursor) availableApp
    assertEqual "" (Just "invalid_cursor") (errorCode conflict)

marksStaleAfterFailedReload :: IO ()
marksStaleAfterFailedReload = do
    store <- loadedStore
    reloadResult <- reloadSnapshot store "test/fixtures/does-not-exist.journal"
    assertBool "reload should fail" (isLeft reloadResult)
    response <- get "/api/v1/transactions" (makeApplication configuration store)
    case eitherDecode response.simpleBody :: Either String TransactionsResponse of
        Left decodeError -> assertFailure decodeError
        Right value -> do
            assertEqual "" "stale" value.metadata.freshness
            assertBool "should have warnings" (not . null $ value.metadata.warnings)
            assertEqual "" 13 (length value.transactions)

reportsUnavailableSnapshot :: Wai.Application -> IO ()
reportsUnavailableSnapshot unavailableApp = do
    live <- get "/api/v1/health/live" unavailableApp
    assertEqual "" status200 (simpleStatus live)
    ready <- getWithRequestId "/api/v1/health/ready" unavailableApp
    assertEqual "" status503 (simpleStatus ready)
    let unavailableJson =
            decode
                ( "{\"error\":{\"code\":\"journal_unavailable\",\
                  \\"message\":\"No usable journal snapshot is available.\",\
                  \\"requestId\":\"req_fixture_0001\"}}"
                )
    assertEqual "" unavailableJson (json ready)
    transactionsResponse <- getWithRequestId "/api/v1/transactions" unavailableApp
    assertEqual "" status503 (simpleStatus transactionsResponse)
    assertEqual "" unavailableJson (json transactionsResponse)

returnsStableNotFound :: Wai.Application -> IO ()
returnsStableNotFound availableApp = do
    missing <- getWithRequestId "/api/v1/missing" availableApp
    assertEqual "" status404 (simpleStatus missing)
    assertEqual "" (Just "application/json; charset=utf-8") (lookup hContentType (simpleHeaders missing))
    let notFoundJson =
            decode
                ( "{\"error\":{\"code\":\"not_found\",\
                  \\"message\":\"Requested API route does not exist.\",\
                  \\"requestId\":\"req_fixture_0001\"}}"
                )
    assertEqual "" notFoundJson (json missing)
