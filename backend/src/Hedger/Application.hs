module Hedger.Application (
    AppM,
    Env (..),
    bootstrap,
    liveness,
    readiness,
    runAppM,
    transactionList,
)
where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Hedger.Config (Config (..))
import Hedger.Domain.Error (AppError (JournalUnavailable))
import Hedger.Domain.Snapshot (Snapshot (..), SnapshotStore, readCurrentSnapshot, readLastReloadError)
import Hedger.Domain.Transaction (projectTransactions)
import Hedger.Domain.Types (Bootstrap (..), Health (..), ResponseMetadata (..), TransactionsResponse (..))

data Env = Env
    { config :: Config
    , snapshotStore :: SnapshotStore
    }

type AppM = ReaderT Env (ExceptT AppError IO)

runAppM :: Env -> AppM value -> IO (Either AppError value)
runAppM environment action = runExceptT (runReaderT action environment)

liveness :: AppM Health
liveness = pure Health{status = "ok"}

readiness :: AppM Health
readiness = requireSnapshot >> pure Health{status = "ok"}

bootstrap :: AppM Bootstrap
bootstrap = do
    snapshot <- requireSnapshot
    fiscal <- asks (.config.fiscalYearStartMonth)
    responseMetadata <- metadataFor snapshot
    pure
        Bootstrap
            { apiVersion = "v1"
            , appName = "Hedger"
            , fiscalYearStartMonth = fiscal
            , defaultValuationBasis = "period-end"
            , metadata = responseMetadata
            }

transactionList :: AppM TransactionsResponse
transactionList = do
    snapshot <- requireSnapshot
    responseMetadata <- metadataFor snapshot
    pure
        TransactionsResponse
            { metadata = responseMetadata
            , transactions = projectTransactions snapshot.transactions
            }

requireSnapshot :: AppM Snapshot
requireSnapshot = do
    store <- asks snapshotStore
    maybeSnapshot <- liftIO (readCurrentSnapshot store)
    case maybeSnapshot of
        Just snapshot -> pure snapshot
        Nothing -> throwError JournalUnavailable

metadataFor :: Snapshot -> AppM ResponseMetadata
metadataFor snapshot = do
    store <- asks snapshotStore
    reloadError <- liftIO (readLastReloadError store)
    pure
        ResponseMetadata
            { revision = snapshot.revision
            , freshness = maybe "fresh" (const "stale") reloadError
            , warnings = maybe [] (const ["The latest journal reload failed; the last usable snapshot is shown."]) reloadError
            }
