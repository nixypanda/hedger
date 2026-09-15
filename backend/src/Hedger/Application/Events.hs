module Hedger.Application.Events (
    RevisionSubscription,
    subscribeToRevisions,
    awaitNextRevision,
)
where

import Hedger.Application.AppM (Env (..))
import Hedger.Infrastructure.RevisionSubscription (
    RevisionSubscription,
    awaitNextRevision,
 )
import Hedger.Infrastructure.RevisionSubscription qualified as RevisionSubscription

subscribeToRevisions :: Env -> IO RevisionSubscription
subscribeToRevisions environment = RevisionSubscription.subscribeToRevisions environment.snapshotStore
