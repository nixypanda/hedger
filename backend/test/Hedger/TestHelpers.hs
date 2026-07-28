module Hedger.TestHelpers (
    configuration,
    loadedStore,
    getJson,
    postJson,
) where

import Data.Aeson (Value, decode)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Either (isRight)
import Network.HTTP.Types (hContentType, methodGet, methodPost)
import Network.Wai qualified as Wai
import Network.Wai.Test (
    SRequest (..),
    SResponse (..),
    defaultRequest,
    request,
    runSession,
    setPath,
    srequest,
 )
import Test.Tasty.HUnit (assertBool)

import Hedger.Config (Config (..))
import Hedger.Domain.Investment (
    AssetClass (Equity),
    AssetClassMapping (..),
    AssetClassMappings,
    emptyAssetClassMappings,
    emptyInvestmentMappings,
    mkAssetClassMappings,
 )
import Hedger.Infrastructure.SnapshotStore (SnapshotStore, newSnapshotStore, reloadSnapshot)

configuration :: Config
configuration =
    Config
        { journalPath = "test/fixtures/shared.journal"
        , listenAddress = "127.0.0.1"
        , listenPort = 8080
        , defaultDisplayCommodity = "INR"
        , fiscalYearStartMonth = 4
        , investmentMappings = emptyInvestmentMappings
        , assetClassMappings = configuredAssetClasses
        }

configuredAssetClasses :: AssetClassMappings
configuredAssetClasses =
    either
        (const emptyAssetClassMappings)
        id
        (mkAssetClassMappings [AssetClassMapping Equity ["assets:investments"] Nothing])

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

postJson ::
    ByteString.ByteString ->
    LazyByteString.ByteString ->
    Wai.Application ->
    IO (Maybe Value, SResponse)
postJson path body app = do
    let requestValue =
            (setPath defaultRequest path)
                { Wai.requestMethod = methodPost
                , Wai.requestHeaders = [(hContentType, "application/json")]
                }
    response <- runSession (srequest (SRequest requestValue body)) app
    pure (decode response.simpleBody, response)
