module Hedger.Application.AppM (
    AppM,
    Env (..),
    SnapshotContext (..),
    runAppM,
    requireSnapshot,
)
where

import Control.Exception (
    SomeAsyncException,
    evaluate,
    fromException,
    throwIO,
    try,
 )
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)

import Hedger.Config (Config (..))
import Hedger.Domain.Error (AppError (InternalError, JournalUnavailable))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.DerivedStore (DerivedStore)
import Hedger.Infrastructure.SnapshotStore (
    SnapshotLoadError,
    SnapshotStore,
    SnapshotStoreRead (..),
    readSnapshotStore,
 )

data Env = Env
    { config :: Config
    , snapshotStore :: SnapshotStore
    , derivedStore :: DerivedStore
    }

type AppM = ReaderT Env (ExceptT AppError IO)

data SnapshotContext = SnapshotContext
    { snapshot :: Snapshot
    , reloadError :: Maybe SnapshotLoadError
    }

runAppM :: Env -> AppM value -> IO (Either AppError value)
runAppM environment action = do
    result <- try (runExceptT (runReaderT action environment) >>= evaluate)
    case result of
        Right value -> pure value
        Left e
            | isAsynchronous e -> throwIO e
            | otherwise -> pure (Left InternalError)
  where
    isAsynchronous e = case fromException e :: Maybe SomeAsyncException of
        Just _ -> True
        Nothing -> False

requireSnapshot :: AppM SnapshotContext
requireSnapshot = do
    store <- asks (.snapshotStore)
    storeRead <- liftIO (readSnapshotStore store)
    case storeRead.snapshot of
        Just snapshot -> pure SnapshotContext{snapshot, reloadError = storeRead.reloadError}
        Nothing -> throwError JournalUnavailable
