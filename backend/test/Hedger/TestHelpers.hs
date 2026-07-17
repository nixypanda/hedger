module Hedger.TestHelpers (
    configuration,
    loadedStore,
    get,
    getWithRequestId,
    getQuery,
    decodeTransactions,
    assertQueryCount,
    errorCode,
    json,
) where

import Data.Aeson (Value (..), decode, eitherDecode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as ByteString
import Data.Either (isRight)
import Data.Text qualified as Text
import Hedger.Config (Config (..), LogLevel (Info))
import Hedger.Infrastructure.Hledger.SnapshotStore (
    SnapshotStore,
    newSnapshotStore,
    reloadSnapshot,
 )
import Hedger.Interface.HTTP.Types (TransactionsResponse (..))
import Network.HTTP.Types (methodGet, status200)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Test (SResponse (..), defaultRequest, request, runSession, setPath)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure)

configuration :: Config
configuration =
    Config
        { journalPath = "test/fixtures/shared.journal"
        , listenAddress = "127.0.0.1"
        , listenPort = 8080
        , defaultDisplayCommodity = "INR"
        , fiscalYearStartMonth = 4
        , logLevel = Info
        , publicBaseUrl = Nothing
        }

loadedStore :: IO SnapshotStore
loadedStore = do
    store <- newSnapshotStore
    result <- reloadSnapshot store "test/fixtures/shared.journal"
    assertBool "snapshot load failed" (isRight result)
    pure store

get :: ByteString.ByteString -> Wai.Application -> IO SResponse
get path app = runSession (request (setPath defaultRequest{Wai.requestMethod = methodGet} path)) app

getWithRequestId :: ByteString.ByteString -> Wai.Application -> IO SResponse
getWithRequestId path app =
    runSession
        (request (setPath defaultRequest{Wai.requestMethod = methodGet, Wai.requestHeaders = [("X-Request-ID", "req_fixture_0001")]} path))
        app

getQuery :: ByteString.ByteString -> Wai.Application -> IO SResponse
getQuery queryString app =
    runSession
        ( request
            ( (setPath defaultRequest "/api/v1/transactions")
                { Wai.requestMethod = methodGet
                , Wai.requestHeaders = [("X-Request-ID", "req_fixture_0001")]
                , Wai.rawQueryString = queryString
                , Wai.queryString = HTTP.parseQuery queryString
                }
            )
        )
        app

decodeTransactions :: SResponse -> IO TransactionsResponse
decodeTransactions response = do
    assertEqual "" status200 (simpleStatus response)
    case eitherDecode response.simpleBody of
        Left decodeError -> assertFailure decodeError
        Right value -> pure value

assertQueryCount :: Wai.Application -> (ByteString.ByteString, Int) -> IO ()
assertQueryCount app (query, expectedCount) = do
    response <- getQuery ("?query=" <> query) app
    value <- decodeTransactions response
    assertEqual "" expectedCount (length value.transactions)

errorCode :: SResponse -> Maybe Text.Text
errorCode response = do
    Object envelope <- decode response.simpleBody
    Object payload <- KeyMap.lookup "error" envelope
    String code <- KeyMap.lookup "code" payload
    pure code

json :: SResponse -> Maybe Value
json = decode . simpleBody
