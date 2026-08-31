module Hedger.Application.Cache (
    cachedResponse,
    dimensionKey,
    encodedSizeHint,
)
where

import Control.Exception (Exception, throwIO, try)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Data.Typeable (Typeable)

import Hedger.Application.AppM (AppM, Env (..), SnapshotContext (..), requireSnapshot, runAppM)
import Hedger.Application.Common (SnapshotMetadata, metadataFor)
import Hedger.Domain.Error (AppError)
import Hedger.Domain.Snapshot (Snapshot (..))
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
    either throwError (pure . patchMetadata (metadataFor context)) result

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

{- | A collision-free cache dimension from the already parsed and defaulted
request fields, encoded as JSON so no field value can imitate a separator.
-}
dimensionKey :: [Aeson.Value] -> Text
dimensionKey = TextEncoding.decodeUtf8Lenient . LazyByteString.toStrict . Aeson.encode

-- | Approximate retained size of a response DTO: the length of its JSON encoding.
encodedSizeHint :: (ToJSON value) => value -> Int
encodedSizeHint = fromIntegral . LazyByteString.length . Aeson.encode
