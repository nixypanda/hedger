module Main (main) where

import Data.Aeson (Value, decode, eitherDecode)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Either (isLeft, isRight)
import Data.List qualified as List
import Data.Maybe (isJust)
import Data.Ord (Down (Down))
import Data.Text qualified as Text
import Data.Time (Day (ModifiedJulianDay), fromGregorian)
import Hedgehog (forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Hedger.Config (Config (..), LogLevel (Info), parseConfig)
import Hedger.Domain.Snapshot (
    Snapshot (latestJournalDate, revision, transactions),
 )
import Hedger.Domain.Transaction (newestFirst)
import Hedger.Infrastructure.Hledger.SnapshotStore (
    SnapshotStore,
    loadSnapshot,
    newSnapshotStore,
    readCurrentSnapshot,
    readLastReloadError,
    reloadSnapshot,
 )
import Hedger.Interface.HTTP.TransactionMapper (toTransactions)
import Hedger.Interface.HTTP.Types qualified as Types
import Hedger.Runtime.Server (makeApplication, snapshotAvailable)
import Hledger.Data.Types qualified as Hledger
import Network.HTTP.Types (hContentType, methodGet, status200, status404, status503)
import Network.Wai qualified as Wai
import Network.Wai.Test (SResponse (simpleBody, simpleHeaders, simpleStatus), defaultRequest, request, runSession, setPath)
import Test.Hspec

main :: IO ()
main = hspec $ do
    availableStore <- runIO loadedStore
    unavailableStore <- runIO newSnapshotStore
    let availableApp = makeApplication configuration availableStore
        unavailableApp = makeApplication configuration unavailableStore

    describe "configuration" $ do
        it "requires a journal runtime path" $
            parseConfig [] [] `shouldBe` Left "A non-empty journal path is required."
        it "validates bounded port and fiscal month" $ do
            parseConfig [("HEDGER_JOURNAL_PATH", "fixture.journal")] ["--port", "0"]
                `shouldBe` Left "Port must be between 1 and 65535."
            parseConfig [("HEDGER_JOURNAL_PATH", "fixture.journal")] ["--fiscal-year-start", "13"]
                `shouldBe` Left "Fiscal-year start must be between 1 and 12."
        it "allows runtime journal paths with spaces and lets CLI values win" $
            fmap (.journalPath) (parseConfig [("HEDGER_JOURNAL_PATH", "/runtime/old journal")] ["--journal", "/runtime/new journal"])
                `shouldBe` Right "/runtime/new journal"
        it "renders optparse-applicative help" $
            parseConfig [] ["--help"] `shouldSatisfy` either (Text.isInfixOf "Usage: hedger-server") (const False)

    describe "fixture readiness" $ do
        it "recognizes the shared journal as usable" $ do
            snapshotAvailable "test/fixtures/shared.journal" `shouldReturn` True
            snapshotAvailable "test/fixtures/does-not-exist.journal" `shouldReturn` False

    describe "journal snapshots" $ do
        it "parses the shared journal with every transaction and posting" $ do
            result <- loadSnapshot "test/fixtures/shared.journal"
            result `shouldSatisfy` isRight
            case result of
                Left loadError -> expectationFailure (show loadError)
                Right snapshot -> do
                    length snapshot.transactions `shouldBe` 13
                    sum (length . Hledger.tpostings <$> snapshot.transactions) `shouldBe` 27
                    snapshot.latestJournalDate `shouldBe` Just (fromGregorian 2024 4 2)
                    snapshot.revision `shouldSatisfy` Text.isPrefixOf "md5:"

        it "derives the same non-placeholder revision from unchanged effective source content" $ do
            first <- loadSnapshot "test/fixtures/shared.journal"
            second <- loadSnapshot "test/fixtures/shared.journal"
            fmap (.revision) first `shouldBe` fmap (.revision) second

        it "rejects malformed journals instead of treating readable files as ready" $ do
            result <- loadSnapshot "test/fixtures/malformed.journal"
            result `shouldSatisfy` isLeft

        it "keeps the last good immutable snapshot when a reload fails" $ do
            store <- loadedStore
            snapshotBefore <- readCurrentSnapshot store
            reloadResult <- reloadSnapshot store "test/fixtures/does-not-exist.journal"
            reloadResult `shouldSatisfy` isLeft
            snapshotAfter <- readCurrentSnapshot store
            fmap (length . (.transactions)) snapshotAfter `shouldBe` fmap (length . (.transactions)) snapshotBefore
            reloadError <- readLastReloadError store
            reloadError `shouldSatisfy` isJust

    describe "transaction projection" $ do
        it "orders newest dates first and reverses journal order within a date" $ do
            result <- loadSnapshot "test/fixtures/shared.journal"
            case result of
                Left loadError -> expectationFailure (show loadError)
                Right snapshot -> do
                    let sameDate = zipWith (\index transaction -> transaction{Hledger.tdate = fromGregorian 2024 1 1, Hledger.tindex = index}) [1 ..] (take 3 snapshot.transactions)
                        sourceIndexes = (.tindex) <$> sameDate
                        orderedIndexes = (.tindex) <$> newestFirst sameDate
                    orderedIndexes `shouldBe` reverse sourceIndexes

        it "orders generated transaction keys by descending date and source position" $ do
            result <- loadSnapshot "test/fixtures/shared.journal"
            case result of
                Left loadError -> expectationFailure (show loadError)
                Right snapshot ->
                    case snapshot.transactions of
                        [] -> expectationFailure "The shared fixture needs a transaction template."
                        template : _ -> Hedgehog.check (orderingProperty template) `shouldReturn` True

        it "projects every transaction, posting, and raw amount exactly once" $ do
            result <- loadSnapshot "test/fixtures/shared.journal"
            case result of
                Left loadError -> expectationFailure (show loadError)
                Right snapshot -> do
                    let projected = toTransactions snapshot.transactions
                    length projected `shouldBe` 13
                    sum (length . (.postings) <$> projected) `shouldBe` 27
                    sum (sum . fmap (length . (.amounts)) . (.postings) <$> projected) `shouldBe` 27
                    fmap (.primaryDate) projected `shouldBe` List.sortOn Down (Hledger.tdate <$> snapshot.transactions)
                    fmap (\transaction -> (transaction.primaryDate, transaction.description, fmap (.account) transaction.postings)) projected
                        `shouldBe` fmap (\transaction -> (transaction.tdate, transaction.tdescription, fmap (.paccount) transaction.tpostings)) (newestFirst snapshot.transactions)

        it "preserves unit and total cost types without manufacturing missing costs" $ do
            result <- loadSnapshot "test/fixtures/shared.journal"
            case result of
                Left loadError -> expectationFailure (show loadError)
                Right snapshot -> do
                    let costs =
                            [ cost.costType
                            | transaction <- toTransactions snapshot.transactions
                            , posting <- transaction.postings
                            , amount <- posting.amounts
                            , Just cost <- [amount.cost]
                            ]
                    costs `shouldBe` ["total", "unit"]

        it "preserves optional dates, statuses, codes, comments, and tags" $ do
            result <- loadSnapshot "test/fixtures/shared.journal"
            case result of
                Left loadError -> expectationFailure (show loadError)
                Right snapshot ->
                    case toTransactions snapshot.transactions of
                        [] -> expectationFailure "The shared fixture needs transactions."
                        transaction : _ -> do
                            transaction.secondaryDate `shouldBe` Just (fromGregorian 2024 4 3)
                            transaction.status `shouldBe` "cleared"
                            transaction.code `shouldBe` Just "UTIL"
                            transaction.comment `shouldSatisfy` maybe False (Text.isInfixOf "category: household")
                            fmap (.name) transaction.tags `shouldBe` ["category"]
                            fmap (.status) transaction.postings `shouldBe` ["cleared", "unmarked"]
                            fmap (fmap (.name) . (.tags)) transaction.postings `shouldBe` [["method"], []]

        it "derives eLedger list classification and exact headline totals in Haskell" $ do
            result <- loadSnapshot "test/fixtures/shared.journal"
            case result of
                Left loadError -> expectationFailure (show loadError)
                Right snapshot -> do
                    let projected = toTransactions snapshot.transactions
                    fmap (.presentation.classification) (take 4 projected)
                        `shouldBe` ["expense", "income", "expense", "income"]
                    case projected of
                        transaction : _ -> do
                            transaction.presentation.fromAccounts `shouldBe` ["assets"]
                            transaction.presentation.toAccounts `shouldBe` ["expenses"]
                            fmap (\amount -> (amount.commodity, amount.quantity)) transaction.presentation.totalAmounts
                                `shouldBe` [("INR", "3000")]
                        [] -> expectationFailure "The shared fixture needs transactions."

    describe "API" $ do
        it "serves live, ready, and bootstrap from the fixture" $ do
            live <- get "/api/v1/health/live" availableApp
            simpleStatus live `shouldBe` status200
            lookup "X-Request-ID" (simpleHeaders live) `shouldSatisfy` maybe False (ByteString.isPrefixOf "req-")
            json live `shouldBe` decode "{\"status\":\"ok\"}"
            ready <- get "/api/v1/health/ready" availableApp
            simpleStatus ready `shouldBe` status200
            bootstrap <- get "/api/v1/bootstrap" availableApp
            simpleStatus bootstrap `shouldBe` status200
            case eitherDecode bootstrap.simpleBody :: Either String Types.Bootstrap of
                Left decodeError -> expectationFailure decodeError
                Right value -> do
                    value.apiVersion `shouldBe` "v1"
                    value.metadata.freshness `shouldBe` "fresh"
                    value.metadata.revision `shouldSatisfy` Text.isPrefixOf "md5:"

        it "serves the complete transaction shape and shared response metadata" $ do
            response <- get "/api/v1/transactions" availableApp
            simpleStatus response `shouldBe` status200
            case eitherDecode response.simpleBody :: Either String Types.TransactionsResponse of
                Left decodeError -> expectationFailure decodeError
                Right value -> do
                    value.metadata.freshness `shouldBe` "fresh"
                    value.metadata.revision `shouldSatisfy` Text.isPrefixOf "md5:"
                    length value.transactions `shouldBe` 13
                    sum (length . (.postings) <$> value.transactions) `shouldBe` 27
                    value.transactions `shouldSatisfy` all (not . Text.null . (.description))

        it "marks retained transaction data stale after a failed reload" $ do
            store <- loadedStore
            reloadResult <- reloadSnapshot store "test/fixtures/does-not-exist.journal"
            reloadResult `shouldSatisfy` isLeft
            response <- get "/api/v1/transactions" (makeApplication configuration store)
            case eitherDecode response.simpleBody :: Either String Types.TransactionsResponse of
                Left decodeError -> expectationFailure decodeError
                Right value -> do
                    value.metadata.freshness `shouldBe` "stale"
                    value.metadata.warnings `shouldSatisfy` (not . null)
                    length value.transactions `shouldBe` 13

        it "matches the representative journal-to-JSON golden" $ do
            response <- get "/api/v1/transactions" availableApp
            expected <- LazyByteString.readFile "test/golden/transactions.json"
            (decode response.simpleBody :: Maybe Value) `shouldBe` decode expected

        it "keeps liveness up but reports an unavailable snapshot" $ do
            live <- get "/api/v1/health/live" unavailableApp
            simpleStatus live `shouldBe` status200
            ready <- getWithRequestId "/api/v1/health/ready" unavailableApp
            simpleStatus ready `shouldBe` status503
            json ready
                `shouldBe` decode "{\"error\":{\"code\":\"journal_unavailable\",\"message\":\"No usable journal snapshot is available.\",\"requestId\":\"req_fixture_0001\"}}"
            transactionsResponse <- getWithRequestId "/api/v1/transactions" unavailableApp
            simpleStatus transactionsResponse `shouldBe` status503
            json transactionsResponse
                `shouldBe` decode "{\"error\":{\"code\":\"journal_unavailable\",\"message\":\"No usable journal snapshot is available.\",\"requestId\":\"req_fixture_0001\"}}"

        it "returns stable JSON for unknown routes" $ do
            missing <- getWithRequestId "/api/v1/missing" availableApp
            simpleStatus missing `shouldBe` status404
            lookup hContentType (simpleHeaders missing) `shouldBe` Just "application/json; charset=utf-8"
            json missing
                `shouldBe` decode "{\"error\":{\"code\":\"not_found\",\"message\":\"The requested API route does not exist.\",\"requestId\":\"req_fixture_0001\"}}"

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

get :: ByteString.ByteString -> Wai.Application -> IO SResponse
get path app = runSession (request (setPath defaultRequest{Wai.requestMethod = methodGet} path)) app

getWithRequestId :: ByteString.ByteString -> Wai.Application -> IO SResponse
getWithRequestId path app =
    runSession
        (request (setPath defaultRequest{Wai.requestMethod = methodGet, Wai.requestHeaders = [("X-Request-ID", "req_fixture_0001")]} path))
        app

json :: SResponse -> Maybe Value
json = decode . simpleBody

loadedStore :: IO SnapshotStore
loadedStore = do
    store <- newSnapshotStore
    result <- reloadSnapshot store "test/fixtures/shared.journal"
    result `shouldSatisfy` isRight
    pure store

orderingProperty :: Hledger.Transaction -> Hedgehog.Property
orderingProperty template = property $ do
    keys <- forAll (Gen.list (Range.linear 0 80) transactionKey)
    let records = (\(day, index) -> template{Hledger.tdate = day, Hledger.tindex = index}) <$> keys
        actual = (\transaction -> (transaction.tdate, transaction.tindex)) <$> newestFirst records
        expected = List.sortOn (\(day, index) -> (Down day, Down index)) keys
    actual === expected

transactionKey :: Hedgehog.Gen (Day, Integer)
transactionKey =
    (,)
        <$> (ModifiedJulianDay <$> Gen.integral (Range.linear 40000 80000))
        <*> Gen.integral (Range.linear 0 100000)
