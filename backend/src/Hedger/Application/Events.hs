module Hedger.Application.Events (
    RevisionSubscription,
    subscribeToRevisions,
    awaitNextRevision,
) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)

import Hedger.Application.AppM (Env (..))
import Hedger.Infrastructure.SnapshotStore (SnapshotStore, awaitRevisionChange, currentRevision)

data RevisionSubscription
    = RevisionSubscription SnapshotStore (IORef Text)

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
