module Hedger.Interface.HTTP.EventsSpec (httpEventsSpec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Text.Encoding qualified as TextEncoding
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), Socket, SocketType (Stream))
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketByteString
import Network.Wai.Handler.Warp (testWithApplication)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

import Hedger.Application.AppM (Env (..))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.SnapshotStore (reloadSnapshot)
import Hedger.Interface.HTTP.Events (HeartbeatInterval (..), eventsApplicationWithHeartbeat)
import Hedger.TestHelpers (configuration, loadedStore, testEnv)

httpEventsSpec :: TestTree
httpEventsSpec =
    testGroup
        "HTTP events"
        [ testCase "sends a heartbeat while the stream is idle" sendsIdleHeartbeat
        , testCase "accepts a revision subscriber after an idle client disconnects" reconnectsAfterDisconnect
        ]

sendsIdleHeartbeat :: IO ()
sendsIdleHeartbeat = withEventsServer $ \port ->
    withClient port $ \client -> do
        response <- receiveContaining "heartbeat" client
        assertBool "heartbeat should be an SSE comment" (":heartbeat" `ByteString.isInfixOf` response)

reconnectsAfterDisconnect :: IO ()
reconnectsAfterDisconnect = do
    store <- loadedStore
    environment <- testEnv store
    let application = eventsApplicationWithHeartbeat testHeartbeat environment
    testWithApplication (pure application) $ \port -> do
        withClient port $ \client -> receiveContaining "heartbeat" client >> pure ()
        threadDelay 50000
        withClient port $ \client -> do
            _ <- receiveContaining "heartbeat" client
            reloadResult <- reloadSnapshot store "test/fixtures/investments.journal"
            case reloadResult of
                Left _ -> assertFailure "replacement snapshot load failed"
                Right snapshot -> do
                    response <- receiveContaining (TextEncoding.encodeUtf8 snapshot.revision) client
                    assertBool
                        "revision event should be named"
                        ("event:revision" `ByteString.isInfixOf` response)

withEventsServer :: (Int -> IO a) -> IO a
withEventsServer action = do
    store <- loadedStore
    environment <- testEnv store
    let application = eventsApplicationWithHeartbeat testHeartbeat environment
    testWithApplication (pure application) action

withClient :: Int -> (Socket -> IO a) -> IO a
withClient port = bracket (connectClient port) Socket.close

connectClient :: Int -> IO Socket
connectClient port = do
    client <- Socket.socket AF_INET Stream Socket.defaultProtocol
    Socket.connect client (SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127, 0, 0, 1)))
    SocketByteString.sendAll client eventRequest
    pure client

eventRequest :: ByteString
eventRequest = "GET /events HTTP/1.1\r\nHost: localhost\r\nAccept: text/event-stream\r\n\r\n"

receiveContaining :: ByteString -> Socket -> IO ByteString
receiveContaining needle client = do
    result <- timeout 1000000 (go "")
    maybe (assertFailure ("timed out waiting for " <> show needle)) pure result
  where
    go received
        | needle `ByteString.isInfixOf` received = pure received
        | otherwise = do
            chunk <- SocketByteString.recv client 4096
            if ByteString.null chunk
                then assertFailure "event stream closed unexpectedly"
                else go (received <> chunk)

testHeartbeat :: HeartbeatInterval
testHeartbeat = HeartbeatInterval 20000
