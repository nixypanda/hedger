module Main (main) where

import Data.Aeson (Value, decode)
import Data.ByteString qualified as ByteString
import Data.Text qualified as Text
import Hedger.Config (Config (..), LogLevel (Info), parseConfig)
import Hedger.Runtime.Server (makeApplication, snapshotAvailable)
import Network.HTTP.Types (hContentType, methodGet, status200, status404, status503)
import Network.Wai qualified as Wai
import Network.Wai.Test (SResponse (simpleBody, simpleHeaders, simpleStatus), defaultRequest, request, runSession, setPath)
import Test.Hspec

main :: IO ()
main = hspec $ do
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
            json bootstrap
                `shouldBe` decode "{\"apiVersion\":\"v1\",\"appName\":\"Hedger\",\"fiscalYearStartMonth\":4,\"defaultValuationBasis\":\"period-end\"}"

        it "keeps liveness up but reports an unavailable snapshot" $ do
            live <- get "/api/v1/health/live" unavailableApp
            simpleStatus live `shouldBe` status200
            ready <- getWithRequestId "/api/v1/health/ready" unavailableApp
            simpleStatus ready `shouldBe` status503
            json ready
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

availableApp :: Wai.Application
availableApp = makeApplication configuration True

unavailableApp :: Wai.Application
unavailableApp = makeApplication configuration False

get :: ByteString.ByteString -> Wai.Application -> IO SResponse
get path app = runSession (request (setPath defaultRequest{Wai.requestMethod = methodGet} path)) app

getWithRequestId :: ByteString.ByteString -> Wai.Application -> IO SResponse
getWithRequestId path app =
    runSession
        (request (setPath defaultRequest{Wai.requestMethod = methodGet, Wai.requestHeaders = [("X-Request-ID", "req_fixture_0001")]} path))
        app

json :: SResponse -> Maybe Value
json = decode . simpleBody
