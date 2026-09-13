module Hedger.Interface.HTTP.Endpoints.Events (
    EventsAPI,
    HeartbeatInterval (..),
    eventsApplicationWithHeartbeat,
    eventsServer,
)
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Data.ByteString.Builder (Builder, byteString)
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppIO)
import Servant

import Hedger.Application.AppM (Env)
import Hedger.Application.Events (RevisionSubscription, awaitNextRevision, subscribeToRevisions)

type EventsAPI = "events" :> Raw

newtype HeartbeatInterval = HeartbeatInterval Int

-- The request-id header is shared by the whole v1 group but does not apply to a
-- raw streaming response, so it is ignored here.
eventsServer :: Env -> Maybe Text -> Server EventsAPI
eventsServer environment _requestId = Tagged (eventsApplicationWithHeartbeat heartbeatInterval environment)

eventsApplicationWithHeartbeat :: HeartbeatInterval -> Env -> Application
eventsApplicationWithHeartbeat interval environment request sendResponse = do
    subscription <- subscribeToRevisions environment
    eventSourceAppIO (nextServerEvent interval subscription) request sendResponse

nextServerEvent :: HeartbeatInterval -> RevisionSubscription -> IO ServerEvent
nextServerEvent (HeartbeatInterval interval) subscription = do
    result <- race (awaitNextRevision subscription) (threadDelay interval)
    pure $ case result of
        Left revision -> ServerEvent (Just (byteString "revision")) Nothing [textData revision]
        Right () -> CommentEvent (byteString "heartbeat")

heartbeatInterval :: HeartbeatInterval
heartbeatInterval = HeartbeatInterval 15000000

textData :: Text -> Builder
textData = byteString . TextEncoding.encodeUtf8
