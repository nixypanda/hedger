module Hedger.Application.AppM (
    AppM,
    Env (..),
    runAppM,
    requireSnapshot,
)
where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)

import Hedger.Config (Config (..))
import Hedger.Domain.Error (AppError (JournalUnavailable))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.SnapshotStore (SnapshotStore, readCurrentSnapshot)

data Env = Env
    { config :: Config
    , snapshotStore :: SnapshotStore
    }

type AppM = ReaderT Env (ExceptT AppError IO)

runAppM :: Env -> AppM value -> IO (Either AppError value)
runAppM environment action = runExceptT (runReaderT action environment)

requireSnapshot :: AppM Snapshot
requireSnapshot = do
    store <- asks (.snapshotStore)
    maybeSnapshot <- liftIO (readCurrentSnapshot store)
    case maybeSnapshot of
        Just snapshot -> pure snapshot
        Nothing -> throwError JournalUnavailable
