module Hedger.Interface.HTTP.APISpec (
    apiSpec,
) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Vector qualified as Vector
import Network.HTTP.Types (status200, status400, status404, status405, status503)
import Network.Wai qualified as Wai
import Network.Wai.Test (SResponse (simpleHeaders, simpleStatus))
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

import Hedger.Application.Common (SnapshotFreshness (..), SnapshotMetadata (..))
import Hedger.Config (Catalog (..), Config (..), LedgerConfig (..), mkLedgerId)
import Hedger.Infrastructure.SnapshotStore (
    SnapshotLoadError (..),
    newSnapshotStore,
    reloadSnapshot,
 )
import Hedger.Interface.HTTP.Core.Common (toResponseMetadata)
import Hedger.Runtime.Internal.Server (makeApplication, makeCatalogApplication)
import Hedger.TestHelpers (configuration, getJson, loadedStore, postJson)

apiSpec :: TestTree
apiSpec =
    withResource setup teardown $ \getApp ->
        testGroup
            "API"
            [ testCase "fetches accounts tree and register" $ do
                app <- getApp
                accountsAndRegister app
            , testCase "fetches transactions with sequential cursor" $ do
                app <- getApp
                transactionsCursor app
            , testCase "fetches net worth, cashflow, and activity projections" $ do
                app <- getApp
                analyticsProjections app
            , testCase "cashflow rejects an invalid shared transaction query" $ do
                app <- getApp
                invalidCashflowQuery app
            , testCase "asset activity applies the shared transaction query" $ do
                app <- getApp
                assetActivityQuery app
            , testCase "explicitly costed investments do not require performance mappings" $ do
                app <- getApp
                investmentsProjection app
            , testCase "renders fresh and stale metadata" metadataRendering
            , testCase "scopes data and cursors to each ledger" catalogRoutes
            , testCase "one unavailable ledger does not hide a healthy one" partialReadiness
            ]

metadataRendering :: IO ()
metadataRendering = do
    let render = Aeson.toJSON . toResponseMetadata . SnapshotMetadata "default" "revision"
        fresh = render Fresh
        stale = render (Stale (JournalReadFailed "bad file"))
        warning = "The latest journal reload failed; the last usable snapshot is shown."
    assertEqual "fresh status" (Just (String "fresh")) (field "freshness" fresh)
    assertEqual "fresh warnings" (Just (Aeson.toJSON ([] :: [Text]))) (field "warnings" fresh)
    assertEqual "stale status" (Just (String "stale")) (field "freshness" stale)
    assertEqual
        "stale warning"
        (Just (Aeson.toJSON ([warning] :: [Text])))
        (field "warnings" stale)

setup :: IO Wai.Application
setup = do
    store <- loadedStore
    pure (makeApplication configuration store)

teardown :: Wai.Application -> IO ()
teardown = const (pure ())

catalogRoutes :: IO ()
catalogRoutes = do
    personal <- loadedStore
    business <- newSnapshotStore "test/fixtures/investments.journal"
    _ <- reloadSnapshot business
    personalId <- either (fail . Text.unpack) pure (mkLedgerId "personal")
    businessId <- either (fail . Text.unpack) pure (mkLedgerId "business")
    let catalog =
            Catalog
                { defaultLedger = Just personalId
                , ledgerOrder = [personalId, businessId]
                , ledgers =
                    Map.fromList
                        [ (personalId, LedgerConfig "Personal" configuration)
                        ,
                            ( businessId
                            , LedgerConfig
                                "Business"
                                configuration{journalPath = "test/fixtures/investments.journal"}
                            )
                        ]
                , listenAddress = "127.0.0.1"
                , listenPort = configuration.listenPort
                }
        app =
            makeCatalogApplication
                catalog
                ( Map.fromList
                    [ (personalId, (LedgerConfig "Personal" configuration, personal))
                    , (businessId, (LedgerConfig "Business" configuration, business))
                    ]
                )
    (Just list, _) <- getJson "/api/v1/ledgers" app
    assertEqual "default ledger" (Just (String "personal")) (field "defaultLedger" list)
    (_, wrongMethod) <- postJson "/api/v1/ledgers" "{}" app
    assertEqual "catalog method" status405 wrongMethod.simpleStatus
    assertEqual "allowed catalog method" (Just "GET") (lookup "Allow" wrongMethod.simpleHeaders)
    let noDefaultApp =
            makeCatalogApplication
                catalog{defaultLedger = Nothing}
                ( Map.fromList
                    [ (personalId, (LedgerConfig "Personal" configuration, personal))
                    , (businessId, (LedgerConfig "Business" configuration, business))
                    ]
                )
    (Just noDefaultList, _) <- getJson "/api/v1/ledgers" noDefaultApp
    assertEqual "absent default is JSON null" (Just Null) (field "defaultLedger" noDefaultList)
    (Just first, _) <- getJson "/api/v1/ledgers/personal/transactions?limit=5" app
    assertEqual
        "personal metadata"
        (Just (String "personal"))
        (field "metadata" first >>= field "ledgerId")
    (Just other, _) <- getJson "/api/v1/ledgers/business/transactions?limit=5" app
    assertEqual
        "business metadata"
        (Just (String "business"))
        (field "metadata" other >>= field "ledgerId")
    (Just report, reportStatus) <-
        postJson
            "/api/v1/ledgers/business/net-worth/query"
            (Aeson.encode (analyticsRequest ""))
            app
    assertEqual "scoped POST status" status200 reportStatus.simpleStatus
    assertEqual
        "scoped POST metadata"
        (Just (String "business"))
        (field "metadata" report >>= field "ledgerId")
    cursor <-
        case field "nextCursor" first of
            Just (String value) -> pure value
            _ -> assertFailure "expected personal cursor" >> pure ""
    (_, wrongCursor) <-
        getJson
            ( "/api/v1/ledgers/business/transactions?limit=5&cursor="
                <> TextEncoding.encodeUtf8 cursor
            )
            app
    assertEqual "cursor from another ledger" status400 wrongCursor.simpleStatus
    (Just missing, unknown) <- getJson "/api/v1/ledgers/missing/transactions" app
    assertEqual "unknown ledger status" status404 unknown.simpleStatus
    assertEqual
        "unknown ledger envelope"
        (Just (String "ledger_not_found"))
        (field "error" missing >>= field "code")

partialReadiness :: IO ()
partialReadiness = do
    healthy <- loadedStore
    missing <- newSnapshotStore "test/fixtures/not-present.journal"
    _ <- reloadSnapshot missing
    healthyId <- either (fail . Text.unpack) pure (mkLedgerId "healthy")
    missingId <- either (fail . Text.unpack) pure (mkLedgerId "missing")
    let healthyLedger = LedgerConfig "Healthy" configuration
        missingLedger = LedgerConfig "Missing" configuration
        catalog =
            Catalog
                { defaultLedger = Just healthyId
                , ledgerOrder = [healthyId, missingId]
                , ledgers = Map.fromList [(healthyId, healthyLedger), (missingId, missingLedger)]
                , listenAddress = "127.0.0.1"
                , listenPort = configuration.listenPort
                }
        app =
            makeCatalogApplication
                catalog
                ( Map.fromList
                    [(healthyId, (healthyLedger, healthy)), (missingId, (missingLedger, missing))]
                )
    (_, ready) <- getJson "/api/v1/health/ready" app
    assertEqual "global readiness" status200 ready.simpleStatus
    let unavailableDefault =
            makeCatalogApplication
                catalog{defaultLedger = Just missingId}
                ( Map.fromList
                    [(healthyId, (healthyLedger, healthy)), (missingId, (missingLedger, missing))]
                )
    (_, notReady) <- getJson "/api/v1/health/ready" unavailableDefault
    assertEqual "default ledger controls readiness" status503 notReady.simpleStatus
    (Just unavailable, _) <- getJson "/api/v1/ledgers/missing/status" app
    assertEqual "missing ledger status" (Just (String "unavailable")) (field "status" unavailable)

accountsAndRegister :: Wai.Application -> IO ()
accountsAndRegister app = do
    (Just tree, _) <- getJson "/api/v1/accounts/tree" app
    case tree of
        Object obj ->
            case KeyMap.lookup "accounts" obj of
                Just (Array accounts) ->
                    assertEqual
                        "account tree should have 5 top-level accounts"
                        5
                        (Vector.length accounts)
                _ -> assertFailure "expected accounts array"
        _ -> assertFailure "expected object"
    (Just reg, _) <- getJson "/api/v1/accounts/assets:cash:checking/register?limit=2" app
    case reg of
        Object obj ->
            case KeyMap.lookup "entries" obj of
                Just (Array entries) ->
                    assertEqual "register should return 2 entries" 2 (Vector.length entries)
                _ -> assertFailure "expected entries array"
        _ -> assertFailure "expected object"

transactionsCursor :: Wai.Application -> IO ()
transactionsCursor app = do
    (Just first, _) <- getJson "/api/v1/transactions?limit=5" app
    cursor <-
        case first of
            Object obj ->
                case KeyMap.lookup "nextCursor" obj of
                    Just (String c) -> pure c
                    _ -> assertFailure "expected nextCursor string" >> pure ""
            _ -> assertFailure "expected object" >> pure ""
    (Just second, _) <-
        getJson ("/api/v1/transactions?limit=5&cursor=" <> TextEncoding.encodeUtf8 cursor) app
    case second of
        Object obj ->
            case KeyMap.lookup "transactions" obj of
                Just (Array txns) -> do
                    assertEqual "second page should have 5 transactions" 5 (Vector.length txns)
                    case (first, Vector.head txns) of
                        (Object firstObj, Object firstTxn) ->
                            case ( KeyMap.lookup "transactions" firstObj
                                 , KeyMap.lookup "description" firstTxn
                                 ) of
                                (Just (Array firstPage), Just (String desc1)) ->
                                    case Vector.head firstPage of
                                        Object firstPageFirst ->
                                            case KeyMap.lookup "description" firstPageFirst of
                                                Just (String desc2) ->
                                                    assertBool
                                                        "pages should contain different \
                                                        \transactions"
                                                        (desc1 /= desc2)
                                                _ -> assertFailure "expected description string"
                                        _ -> assertFailure "expected object"
                                _ -> assertFailure "expected transactions array with description"
                        _ -> assertFailure "expected objects"
                _ -> assertFailure "expected transactions array"
        _ -> assertFailure "expected object"

analyticsProjections :: Wai.Application -> IO ()
analyticsProjections app = do
    netWorthProjection app
    cashflowProjection app
    activityProjection app

netWorthProjection :: Wai.Application -> IO ()
netWorthProjection app = do
    (Just response, raw) <-
        postJson "/api/v1/net-worth/query" (Aeson.encode (analyticsRequest "")) app
    assertEqual "net worth status" status200 raw.simpleStatus
    assertArrayLength "net worth monthly timeline excludes empty months" 5 "timeline" response
    assertTimelineCompleteness "2023-04" "partial" ["FUND", "JPY"] response
    assertObjectFields
        [ "netWorth"
        , "assets"
        , "liabilities"
        , "assetsBreakdown"
        , "liabilitiesBreakdown"
        , "performance"
        ]
        response

cashflowProjection :: Wai.Application -> IO ()
cashflowProjection app = do
    (Just response, raw) <-
        postJson "/api/v1/cashflow/query" (Aeson.encode (analyticsRequest "acct:expenses")) app
    assertEqual "cashflow status" status200 raw.simpleStatus
    assertEqual
        "cashflow remains available when unrelated assets lack prices"
        (Just (Bool True))
        (field "timelineAvailable" response)
    assertObjectFields
        ["totalCount", "filteredCount", "timeline", "incomeBreakdown", "expenseBreakdown"]
        response

activityProjection :: Wai.Application -> IO ()
activityProjection app = do
    let requestBody =
            object
                [ "kind" .= ("expense" :: String)
                , "range" .= ("month" :: String)
                , "period" .= ("2024-04" :: String)
                , "query" .= ("" :: String)
                , "depth" .= (2 :: Int)
                ]
    (Just response, raw) <- postJson "/api/v1/activity/query" (Aeson.encode requestBody) app
    assertEqual "activity status" status200 raw.simpleStatus
    assertObjectFields ["period", "buckets", "fiscalYearStartMonth"] response
    case field "buckets" response of
        Just (Array buckets)
            | not (Vector.null buckets) -> do
                assertObjectFields
                    ["posted", "reversed", "events", "transactions"]
                    (Vector.head buckets)
                assertEqual
                    "expense activity has no debt movement"
                    (Just (String "0"))
                    (field "netDebtMovement" (Vector.head buckets))
        _ -> assertFailure "expected expense activity buckets"

assetActivityQuery :: Wai.Application -> IO ()
assetActivityQuery app = do
    let requestBody =
            object
                [ "kind" .= ("asset" :: String)
                , "range" .= ("month" :: String)
                , "period" .= ("2024-04" :: String)
                , "query" .= ("acct:expenses" :: String)
                ]
    (Just response, raw) <- postJson "/api/v1/activity/query" (Aeson.encode requestBody) app
    assertEqual "asset query status" status200 raw.simpleStatus
    assertEqual
        "expense-matched transactions have no asset movement"
        (Just (Array Vector.empty))
        (field "buckets" response)

invalidCashflowQuery :: Wai.Application -> IO ()
invalidCashflowQuery app = do
    (_, raw) <-
        postJson "/api/v1/cashflow/query" (Aeson.encode (analyticsRequest "date:not-a-date")) app
    assertEqual "invalid query status" status400 raw.simpleStatus

investmentsProjection :: Wai.Application -> IO ()
investmentsProjection app = do
    let requestBody =
            object
                [ "granularity" .= ("monthly" :: String)
                , "displayCommodity" .= ("INR" :: String)
                , "accountPrefix" .= ("assets:investments:stocks" :: String)
                ]
    (Just response, raw) <- postJson "/api/v1/investments/query" (Aeson.encode requestBody) app
    assertEqual "investments status" status200 raw.simpleStatus
    assertArrayLength "investments monthly timeline includes empty months" 13 "timeline" response
    assertObjectFields
        ["summary", "children", "assetClassAllocations", "breadcrumbs", "warnings"]
        response
    assertPortfolioAssetClassAllocations app response
    case field "summary" response of
        Just summary -> do
            assertEqual "market value" (Just (String "165000")) (field "marketValue" summary)
            assertEqual "purchase cost" (Just (String "120000")) (field "purchaseCost" summary)
            assertEqual "gain" (Just (String "45000")) (field "gainLoss" summary)
            assertBool
                "return percentage is replaced by XIRR"
                (field "returnPercentage" summary == Nothing)
            assertBool "XIRR is present as an annual fraction" (field "xirr" summary /= Nothing)
            assertEqual
                "total profit defaults to the calculated gain"
                (Just (String "45000"))
                (field "totalProfit" summary)
            assertEqual
                "explicit cost is complete without a performance mapping"
                (Just (String "complete"))
                (field "completeness" summary)
        _ -> assertFailure "expected investment summary"
    let childrenRequest =
            object
                [ "granularity" .= ("monthly" :: String)
                , "displayCommodity" .= ("INR" :: String)
                , "accountPrefix" .= ("assets:investments" :: String)
                ]
    (Just childrenResponse, childrenRaw) <-
        postJson "/api/v1/investments/query" (Aeson.encode childrenRequest) app
    assertEqual "investment children status" status200 childrenRaw.simpleStatus
    case field "children" childrenResponse of
        Just (Array children) ->
            assertBool
                "children include asset class and allocation fields"
                (all hasAssetClassAndAllocation children)
        _ -> assertFailure "expected investment children"
    let assetRootRequest =
            object
                [ "granularity" .= ("monthly" :: String)
                , "accountPrefix" .= ("assets:cash" :: String)
                ]
    (Just assetRootResponse, assetRootRaw) <-
        postJson "/api/v1/investments/query" (Aeson.encode assetRootRequest) app
    assertEqual "an omitted asset root is accepted" status200 assetRootRaw.simpleStatus
    case field "summary" assetRootResponse of
        Just omittedSummary ->
            assertEqual
                "an omitted asset root has no investment market value"
                (Just Null)
                (field "marketValue" omittedSummary)
        _ -> assertFailure "expected omitted asset summary"

hasAssetClassAndAllocation :: Value -> Bool
hasAssetClassAndAllocation child =
    field "assetClass" child == Just (String "equity")
        && field "allocationPercentage" child /= Nothing

assertPortfolioAssetClassAllocations :: Wai.Application -> Value -> IO ()
assertPortfolioAssetClassAllocations app scopedResponse = do
    let requestBody =
            object
                [ "granularity" .= ("monthly" :: String)
                , "displayCommodity" .= ("INR" :: String)
                ]
    (Just portfolioResponse, raw) <-
        postJson "/api/v1/investments/query" (Aeson.encode requestBody) app
    assertEqual "portfolio investments status" status200 raw.simpleStatus
    let portfolioAllocations = field "assetClassAllocations" portfolioResponse
        scopedAllocations = field "assetClassAllocations" scopedResponse
    case portfolioAllocations of
        Just (Array allocations) ->
            assertBool
                "portfolio asset class allocations are non-empty"
                (not (Vector.null allocations))
        _ -> assertFailure "expected portfolio asset class allocations"
    assertEqual
        "scoped asset class allocations match the portfolio"
        portfolioAllocations
        scopedAllocations

analyticsRequest :: String -> Value
analyticsRequest query =
    object
        [ "query" .= query
        , "granularity" .= ("monthly" :: String)
        , "positiveDepth" .= (2 :: Int)
        , "negativeDepth" .= (2 :: Int)
        ]

field :: Key -> Value -> Maybe Value
field name value =
    case value of
        Object objectValue -> KeyMap.lookup name objectValue
        _ -> Nothing

assertObjectFields :: [Key] -> Value -> IO ()
assertObjectFields names value =
    assertBool
        "expected analytics response fields"
        (all (\name -> field name value /= Nothing) names)

assertArrayLength :: String -> Int -> Key -> Value -> IO ()
assertArrayLength label expected name value =
    case field name value of
        Just (Array values) -> assertEqual label expected (Vector.length values)
        _ -> assertFailure (label <> ": expected array")

assertTimelineCompleteness :: Text -> Text -> [Text] -> Value -> IO ()
assertTimelineCompleteness period expectedCompleteness expectedMissing response =
    case field "timeline" response of
        Just (Array points) ->
            case Vector.find ((== Just (String period)) . field "period") points of
                Just point -> do
                    assertEqual
                        "timeline point completeness"
                        (Just (String expectedCompleteness))
                        (field "completeness" point)
                    assertEqual
                        "timeline point missing commodities"
                        (Just (Aeson.toJSON expectedMissing))
                        (field "missingCommodities" point)
                Nothing -> assertFailure ("expected timeline point " <> Text.unpack period)
        _ -> assertFailure "expected timeline array"
