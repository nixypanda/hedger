module Hedger.Application (
    AppM,
    Env (..),
    bootstrap,
    liveness,
    readiness,
    runAppM,
)
where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Hedger.Config (Config (..))
import Hedger.Domain.Error (AppError (JournalUnavailable))
import Hedger.Domain.Types (Bootstrap (..), Health (..))

data Env = Env
    { config :: Config
    , hasUsableSnapshot :: Bool
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
    requireSnapshot
    fiscal <- asks (.config.fiscalYearStartMonth)
    pure
        Bootstrap
            { apiVersion = "v1"
            , appName = "Hedger"
            , fiscalYearStartMonth = fiscal
            , defaultValuationBasis = "period-end"
            }

requireSnapshot :: AppM ()
requireSnapshot = do
    available <- asks hasUsableSnapshot
    if available then pure () else throwError JournalUnavailable
