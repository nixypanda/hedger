module Hedger.Interface.HTTP.Events (
    EventsAPI,
    eventsServer,
)
where

import Data.ByteString.Builder (Builder, byteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppIO)
import Servant

import Hedger.Application.AppM (Env (..))
import Hedger.Infrastructure.SnapshotStore (
    SnapshotStore,
    awaitRevisionChange,
    currentRevision,
 )

type EventsAPI = "events" :> Raw

-- The request-id header is shared by the whole v1 group but does not apply to a
-- raw streaming response, so it is ignored here.
eventsServer :: Env -> Maybe Text -> Server EventsAPI
eventsServer environment _requestId = Tagged (snapshotEvents environment.snapshotStore)

-- Each connection remembers the revision it last emitted and blocks until the
-- store publishes a different one, so an idle stream costs nothing.
snapshotEvents :: SnapshotStore -> Application
snapshotEvents store request sendResponse = do
    seen <- newIORef =<< currentRevision store
    eventSourceAppIO (nextRevisionEvent store seen) request sendResponse

nextRevisionEvent :: SnapshotStore -> IORef Text -> IO ServerEvent
nextRevisionEvent store seen = do
    lastSeen <- readIORef seen
    revision <- awaitRevisionChange store lastSeen
    writeIORef seen revision
    pure (ServerEvent (Just (byteString "revision")) Nothing [textData revision])

textData :: Text -> Builder
textData = byteString . TextEncoding.encodeUtf8
