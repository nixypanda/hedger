module Hedger.TestHelpers (
    configuration,
    loadedStore,
    testDerivedParts,
    testEnv,
    testDerivedTargets,
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

import Hedger.Application.AppM (Env (..))
import Hedger.Config (Config (..))
import Hedger.Domain.Investment (
    AssetClass (Equity),
    AssetClassMapping (..),
    AssetClassMappings,
    emptyAssetClassMappings,
    emptyInvestmentMappings,
    mkAssetClassMappings,
 )
import Hedger.Infrastructure.DerivedRevision (DerivedState, newDerivedState)
import Hedger.Infrastructure.DerivedStore (DerivedStore, newDerivedStore)
import Hedger.Infrastructure.JournalWatcher (DerivedTargets (..))
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
        , shadowVerification = False
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

-- | 8 MiB is ample for a test: every spec caches a handful of small responses.
testDerivedParts :: IO (DerivedStore, DerivedState)
testDerivedParts = (,) <$> newDerivedStore (8 * 1024 * 1024) <*> newDerivedState

testEnv :: SnapshotStore -> IO Env
testEnv store = do
    (derivedStore, derivedState) <- testDerivedParts
    pure Env{config = configuration, snapshotStore = store, derivedStore, derivedState}

testDerivedTargets :: IO DerivedTargets
testDerivedTargets = do
    (derivedStore, derivedState) <- testDerivedParts
    pure DerivedTargets{derivedState, derivedStore, config = configuration}
