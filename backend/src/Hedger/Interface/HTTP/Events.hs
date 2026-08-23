module Hedger.Interface.HTTP.Events (
    EventsAPI,
    eventsServer,
)
where

import Data.ByteString.Builder (Builder, byteString)
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppIO)
import Servant

import Hedger.Application.AppM (Env)
import Hedger.Application.Events (RevisionSubscription, awaitNextRevision, subscribeToRevisions)

type EventsAPI = "events" :> Raw

-- The request-id header is shared by the whole v1 group but does not apply to a
-- raw streaming response, so it is ignored here.
eventsServer :: Env -> Maybe Text -> Server EventsAPI
eventsServer environment _requestId = Tagged (snapshotEvents environment)

-- Each connection remembers the revision it last emitted and blocks until the
-- store publishes a different one, so an idle stream costs nothing.
snapshotEvents :: Env -> Application
snapshotEvents environment request sendResponse = do
    subscription <- subscribeToRevisions environment
    eventSourceAppIO (nextRevisionEvent subscription) request sendResponse

nextRevisionEvent :: RevisionSubscription -> IO ServerEvent
nextRevisionEvent subscription = do
    revision <- awaitNextRevision subscription
    pure (ServerEvent (Just (byteString "revision")) Nothing [textData revision])

textData :: Text -> Builder
textData = byteString . TextEncoding.encodeUtf8
