module Hedger.Application.Cache (
    cachedResponse,
    dimensionKey,
    encodedSizeHint,
    usableCheckpoints,
    contributeCheckpoints,
)
where

import Control.Exception (Exception, throwIO, try)
import Control.Monad (void)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Data.Typeable (Typeable)

import Hedger.Application.AppM (AppM, Env (..), SnapshotContext (..), requireSnapshot, runAppM)
import Hedger.Application.Common (SnapshotMetadata, currentMetadata)
import Hedger.Domain.Error (AppError)
import Hedger.Domain.Fingerprint (PrefixChain)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.DerivedRevision (
    DerivedCaches,
    DerivedView (..),
    mergeDerivedCaches,
    readDerivedView,
 )
import Hedger.Infrastructure.DerivedStore (
    CacheKey (..),
    DerivedStore,
    invalidateOtherRevisions,
    lookupOrCompute,
 )

{- | Carries a producer's 'AppError' out through 'lookupOrCompute'\'s exception
path so the failed computation is discarded instead of being memoised: the
cache must only ever hold successful responses, never an error.
-}
newtype ProducerFailed = ProducerFailed AppError
    deriving stock (Show)

instance Exception ProducerFailed

{- | Memoise a response DTO per (report family, snapshot revision, dimensions).

The cached body is reused, but the response metadata is always recomputed from
the live 'SnapshotContext' and patched in, because freshness and reload
warnings change without changing the revision a hit is keyed on.
-}
cachedResponse ::
    (Typeable value) =>
    Text ->
    Text ->
    (SnapshotMetadata -> value -> value) ->
    (value -> Int) ->
    AppM value ->
    AppM value
cachedResponse family dimensions patchMetadata sizeHint produce = do
    context <- requireSnapshot
    environment <- ask
    let key = CacheKey{family, revision = context.snapshot.revision, dimensions}
    -- Unconditional: 'Env' has nowhere to remember the last observed revision, and
    -- dropping entries of superseded revisions is cheap relative to a report.
    liftIO (invalidateOtherRevisions environment.derivedStore context.snapshot.revision)
    result <- liftIO (runCached environment.derivedStore key sizeHint (runAppM environment produce))
    metadata <- currentMetadata context
    either throwError (pure . patchMetadata metadata) result

runCached ::
    (Typeable value) =>
    DerivedStore ->
    CacheKey ->
    (value -> Int) ->
    IO (Either AppError value) ->
    IO (Either AppError value)
runCached store key sizeHint produce = do
    outcome <- try (lookupOrCompute store key sizeHint (produce >>= either (throwIO . ProducerFailed) pure))
    pure $ case outcome of
        Left (ProducerFailed appError) -> Left appError
        Right value -> Right value

{- | The checkpoint caches a request may resume from, with the prefix chain they
are keyed against — or 'Nothing' when the request has to compute unchecked.

There are two ways it is 'Nothing', and both must fall back rather than resume:

* No chain has been published. The derived view is empty until the first rebuild
  completes, and a request arriving before that still has to answer.
* The published revision is not this snapshot's. The caches then describe a
  different journal prefix, and the chain a resume validates its keys against
  would not be the chain the response is being built from — so a checkpoint
  covering a month that /did/ change would still hash-match the stale chain and
  be accepted. This is the same class of guard as the checkpoint hash itself,
  and it is what keeps one response inside a single snapshot revision.
-}
usableCheckpoints :: Snapshot -> AppM (Maybe (PrefixChain, DerivedCaches))
usableCheckpoints snapshot = do
    derivedState <- asks (.derivedState)
    derivedView <- liftIO (readDerivedView derivedState)
    pure $ do
        chain <- derivedView.chain
        if derivedView.revision == snapshot.revision
            then Just (chain, derivedView.caches)
            else Nothing

{- | Contribute a request's updated checkpoints back to the derived view.

The merge is discarded when the published revision has moved on while this
request was computing: those checkpoints describe a superseded chain, and
dropping them is the correct outcome rather than a failure to report. Under a
fixed chain the maps are monotone, so a merge that does land is idempotent and
independent of the order requests complete in.
-}
contributeCheckpoints :: Snapshot -> (DerivedCaches -> DerivedCaches) -> AppM ()
contributeCheckpoints snapshot update = do
    derivedState <- asks (.derivedState)
    void (liftIO (mergeDerivedCaches derivedState snapshot.revision update))

{- | A collision-free cache dimension from the already parsed and defaulted
request fields, encoded as JSON so no field value can imitate a separator.
-}
dimensionKey :: [Aeson.Value] -> Text
dimensionKey = TextEncoding.decodeUtf8Lenient . LazyByteString.toStrict . Aeson.encode

-- | Approximate retained size of a response DTO: the length of its JSON encoding.
encodedSizeHint :: (ToJSON value) => value -> Int
encodedSizeHint = fromIntegral . LazyByteString.length . Aeson.encode
