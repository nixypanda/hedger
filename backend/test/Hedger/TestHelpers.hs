module Hedger.TestHelpers (
    configuration,
    loadedStore,
    getJson,
) where

import Data.Aeson (Value, decode)
import Data.ByteString qualified as ByteString
import Data.Either (isRight)
import Network.HTTP.Types (methodGet)
import Network.Wai qualified as Wai
import Network.Wai.Test (SResponse (..), defaultRequest, request, runSession, setPath)
import Test.Tasty.HUnit (assertBool)

import Hedger.Config (Config (..), LogLevel (Info))
import Hedger.Infrastructure.SnapshotStore (SnapshotStore, newSnapshotStore, reloadSnapshot)

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

getJson :: ByteString.ByteString -> Wai.Application -> IO (Maybe Value, SResponse)
getJson path app = do
    response <- runSession (request (setPath defaultRequest{Wai.requestMethod = methodGet} path)) app
    pure (decode response.simpleBody, response)
