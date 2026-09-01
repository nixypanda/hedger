{- | The revision stream clients follow to know when to refetch.

It is gated on the /derived/ revision, not the snapshot revision, and that is the
whole point of the module. The snapshot is published the instant it parses, so
telling clients then would send every one of them at a derived view that does not
exist yet — the cold-cache stampede the caching design exists to avoid. Waiting
for the derived view means a woken client refetches into a warm one.

The snapshot itself is unaffected: transactions, accounts and the register are
served from it and stay as fresh as they ever were, and a client that polls
instead of subscribing still sees @refreshing@ freshness during the gap.
-}
module Hedger.Application.Events (
    RevisionSubscription,
    subscribeToRevisions,
    awaitNextRevision,
)
where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)

import Hedger.Application.AppM (Env (..))
import Hedger.Infrastructure.DerivedRevision (
    DerivedNotice (..),
    DerivedState,
    awaitDerivedNotice,
    readDerivedNotice,
 )

{- | One client's position in the stream, held as the last tick it was told about.
The 'IORef' is private to the connection, so it needs no synchronisation of its
own; the ordering that matters is inside 'awaitDerivedNotice'.
-}
data RevisionSubscription = RevisionSubscription DerivedState (IORef Int)

subscribeToRevisions :: Env -> IO RevisionSubscription
subscribeToRevisions environment = do
    notice <- readDerivedNotice environment.derivedState
    seen <- newIORef notice.tick
    pure (RevisionSubscription environment.derivedState seen)

awaitNextRevision :: RevisionSubscription -> IO Text
awaitNextRevision (RevisionSubscription state seen) = do
    lastSeen <- readIORef seen
    notice <- awaitDerivedNotice state lastSeen
    writeIORef seen notice.tick
    pure notice.revision
