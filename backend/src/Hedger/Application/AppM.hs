module Hedger.Application.AppM (
    AppM,
    AccountsTree (..),
    AccountRegister (..),
    BootstrapInfo (..),
    Env (..),
    SnapshotMetadata (..),
    RegisterRequest (..),
    TransactionList (..),
    bootstrap,
    accountsTree,
    accountRegister,
    liveness,
    readiness,
    runAppM,
    transactionList,
)
where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, fromGregorian)
import Hedger.Application.AccountRegisterQuery qualified as AccountRegisterQuery
import Hedger.Application.TransactionQuery qualified as TransactionQuery
import Hedger.Config (Config (..))
import Hedger.Domain.Error (AppError (JournalUnavailable))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.SnapshotStore (SnapshotStore, readCurrentSnapshot, readLastReloadError)
import Hledger.Data.Types qualified as Hledger

data SnapshotMetadata = SnapshotMetadata
    { revision :: Text
    , freshness :: Text
    , warnings :: [Text]
    }

data BootstrapInfo = BootstrapInfo
    { apiVersion :: Text
    , appName :: Text
    , fiscalYearStartMonth :: Int
    , metadata :: SnapshotMetadata
    }

data AccountsTree = AccountsTree
    { metadata :: SnapshotMetadata
    , tree :: Hledger.Account Hledger.BalanceData
    , journal :: Hledger.Journal
    , displayCommodity :: Text
    , valuationDate :: Day
    }

data TransactionList = TransactionList
    { metadata :: SnapshotMetadata
    , transactions :: [Hledger.Transaction]
    , nextCursor :: Maybe Text
    }

data RegisterRequest = RegisterRequest
    { account :: Text
    , commodity :: Maybe Text
    , from :: Maybe Text
    , to :: Maybe Text
    , limit :: Maybe Text
    , cursor :: Maybe Text
    }

data AccountRegister = AccountRegister
    { metadata :: SnapshotMetadata
    , displayCommodity :: Text
    , entries :: [AccountRegisterQuery.RegisterEntry]
    , nextCursor :: Maybe Text
    }

data Env = Env
    { config :: Config
    , snapshotStore :: SnapshotStore
    }

type AppM = ReaderT Env (ExceptT AppError IO)

runAppM :: Env -> AppM value -> IO (Either AppError value)
runAppM environment action = runExceptT (runReaderT action environment)

liveness :: AppM ()
liveness = pure ()

readiness :: AppM ()
readiness = requireSnapshot >> pure ()

bootstrap :: AppM BootstrapInfo
bootstrap = do
    snapshot <- requireSnapshot
    fiscal <- asks (.config.fiscalYearStartMonth)
    responseMetadata <- metadataFor snapshot
    pure
        BootstrapInfo
            { apiVersion = "v1"
            , appName = "Hedger"
            , fiscalYearStartMonth = fiscal
            , metadata = responseMetadata
            }

accountsTree :: Maybe Text -> AppM AccountsTree
accountsTree requestedCommodity = do
    snapshot <- requireSnapshot
    configuredCommodity <- asks (.config.defaultDisplayCommodity)
    responseMetadata <- metadataFor snapshot
    pure
        AccountsTree
            { metadata = responseMetadata
            , tree = snapshot.accountTree
            , journal = snapshot.journal
            , displayCommodity = fromMaybe configuredCommodity (nonEmpty requestedCommodity)
            , valuationDate = fromMaybe (fromGregorian 1970 1 1) snapshot.latestJournalDate
            }

transactionList :: Maybe Text -> Maybe Text -> Maybe Text -> AppM TransactionList
transactionList query limit cursor = do
    snapshot <- requireSnapshot
    page <- either throwError pure (TransactionQuery.transactionPage snapshot query limit cursor)
    responseMetadata <- metadataFor snapshot
    pure
        TransactionList
            { metadata = responseMetadata
            , transactions = page.transactions
            , nextCursor = page.nextCursor
            }

accountRegister :: RegisterRequest -> AppM AccountRegister
accountRegister request = do
    snapshot <- requireSnapshot
    configuredCommodity <- asks (.config.defaultDisplayCommodity)
    let resolvedCommodity = fromMaybe configuredCommodity (nonEmpty request.commodity)
    page <-
        either throwError pure $
            AccountRegisterQuery.registerPage
                snapshot
                request.account
                resolvedCommodity
                request.from
                request.to
                request.limit
                request.cursor
    responseMetadata <- metadataFor snapshot
    pure
        AccountRegister
            { metadata = responseMetadata
            , displayCommodity = resolvedCommodity
            , entries = page.entries
            , nextCursor = page.nextCursor
            }

requireSnapshot :: AppM Snapshot
requireSnapshot = do
    store <- asks snapshotStore
    maybeSnapshot <- liftIO (readCurrentSnapshot store)
    case maybeSnapshot of
        Just snapshot -> pure snapshot
        Nothing -> throwError JournalUnavailable

metadataFor :: Snapshot -> AppM SnapshotMetadata
metadataFor snapshot = do
    store <- asks snapshotStore
    reloadError <- liftIO (readLastReloadError store)
    pure
        SnapshotMetadata
            { revision = snapshot.revision
            , freshness = maybe "fresh" (const "stale") reloadError
            , warnings =
                maybe
                    []
                    (const ["The latest journal reload failed; the last usable snapshot is shown."])
                    reloadError
            }

nonEmpty :: Maybe Text -> Maybe Text
nonEmpty value = do
    text <- value
    if Text.null text then Nothing else Just text
