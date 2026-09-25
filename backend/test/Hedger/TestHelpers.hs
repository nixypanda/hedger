module Hedger.TestHelpers (
    configuration,
    loadedStore,
    getJson,
    postJson,
    sumQuantities,
) where

import Data.Aeson (Value, decode)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Decimal (Decimal)
import Data.Either (isRight)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
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

import Hedger.Config (Config (..), mkMonthOfYear, mkPort)
import Hedger.Domain.Investment (
    AssetClass (Equity),
    AssetClassMapping (..),
    AssetClassMappings,
    emptyAssetClassMappings,
    emptyInvestmentMappings,
    mkAssetAccount,
    mkAssetClassMappings,
 )
import Hedger.Domain.NetWorth (BreakdownItem (..))
import Hedger.Infrastructure.SnapshotStore (SnapshotStore, newSnapshotStore, reloadSnapshot)

configuration :: Config
configuration =
    Config
        { journalPath = "test/fixtures/shared.journal"
        , listenAddress = "127.0.0.1"
        , listenPort = either (error . Text.unpack) id (mkPort 8080)
        , defaultDisplayCommodity = "INR"
        , fiscalYearStartMonth = either (error . Text.unpack) id (mkMonthOfYear 4)
        , investmentMappings = emptyInvestmentMappings
        , assetClassMappings = configuredAssetClasses
        }

configuredAssetClasses :: AssetClassMappings
configuredAssetClasses =
    either
        (const emptyAssetClassMappings)
        id
        ( mkAssetClassMappings
            [ AssetClassMapping
                Equity
                (either (error . show) id (mkAssetAccount "assets:investments") :| [])
                Nothing
            ]
        )

loadedStore :: IO SnapshotStore
loadedStore = do
    store <- newSnapshotStore "test/fixtures/shared.journal"
    result <- reloadSnapshot store
    assertBool "snapshot load failed" (isRight result)
    pure store

getJson :: ByteString.ByteString -> Wai.Application -> IO (Maybe Value, SResponse)
getJson path app = do
    response <-
        runSession
            (request (setPath defaultRequest{Wai.requestMethod = methodGet} path))
            app
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

{- | The summed magnitude of a depth-collapsed breakdown, used by the
conservation properties to check that regrouping by display depth does not
change the total.
-}
sumQuantities :: [BreakdownItem] -> Decimal
sumQuantities = sum . fmap (.quantity)
