module Hedger.Infrastructure.RevisionSubscription (
    RevisionSubscription,
    subscribeToRevisions,
    awaitNextRevision,
)
where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)

import Hedger.Infrastructure.SnapshotStore (SnapshotStore, awaitRevisionChange, currentRevision)

data RevisionSubscription = RevisionSubscription SnapshotStore (IORef Text)

subscribeToRevisions :: SnapshotStore -> IO RevisionSubscription
subscribeToRevisions store = do
    seen <- newIORef =<< currentRevision store
    pure (RevisionSubscription store seen)

awaitNextRevision :: RevisionSubscription -> IO Text
awaitNextRevision (RevisionSubscription store seen) = do
    lastSeen <- readIORef seen
    revision <- awaitRevisionChange store lastSeen
    writeIORef seen revision
    pure revision
