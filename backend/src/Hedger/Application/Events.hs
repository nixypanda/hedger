module Hedger.Application.Events (
    RevisionSubscription,
    subscribeToRevisions,
    awaitNextRevision,
    StateSubscription,
    EventState (..),
    subscribeToStates,
    awaitNextState,
) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)

import Hedger.Application.AppM (Env (..))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.SnapshotStore (
    SnapshotStore,
    SnapshotStoreRead (..),
    awaitRevisionChange,
    awaitStateChange,
    currentGeneration,
    currentRevision,
 )

data RevisionSubscription
    = RevisionSubscription SnapshotStore (IORef Text)

data StateSubscription
    = StateSubscription SnapshotStore (IORef Int)

data EventState
    = EventRevision Text
    | EventStale
    | EventUnavailable

subscribeToRevisions :: Env -> IO RevisionSubscription
subscribeToRevisions environment = do
    seen <- newIORef =<< currentRevision environment.snapshotStore
    pure (RevisionSubscription environment.snapshotStore seen)

awaitNextRevision :: RevisionSubscription -> IO Text
awaitNextRevision (RevisionSubscription store seen) = do
    lastSeen <- readIORef seen
    revision <- awaitRevisionChange store lastSeen
    writeIORef seen revision
    pure revision

subscribeToStates :: Env -> IO StateSubscription
subscribeToStates environment = do
    seen <- newIORef =<< currentGeneration environment.snapshotStore
    pure (StateSubscription environment.snapshotStore seen)

awaitNextState :: StateSubscription -> IO EventState
awaitNextState (StateSubscription store seen) = do
    lastSeen <- readIORef seen
    (generation, state) <- awaitStateChange store lastSeen
    writeIORef seen generation
    pure $
        case state of
            SnapshotFresh snapshot -> EventRevision snapshot.revision
            SnapshotStale _ _ -> EventStale
            SnapshotLoadFailed _ -> EventUnavailable
            SnapshotNotLoaded -> EventUnavailable
