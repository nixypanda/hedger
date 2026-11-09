module Hedger.Runtime.Internal.Server (
    makeApplication,
    makeCatalogApplication,
    productionMiddleware,
    runServer,
) where

import Control.Monad (void)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Network.Wai (Application, Middleware)
import Network.Wai.Handler.Warp (
    defaultSettings,
    runSettings,
    setBeforeMainLoop,
    setGracefulShutdownTimeout,
    setHost,
    setInstallShutdownHandler,
    setPort,
 )
import System.Posix.Signals (Handler (Catch), installHandler, sigHUP, sigTERM)

import Hedger.Application.AppM (Env (..))
import Hedger.Config (
    Catalog (..),
    Config (..),
    LedgerConfig (..),
    LedgerId,
    ledgerIdText,
    portValue,
 )
import Hedger.Infrastructure.JournalWatcher (withJournalWatcherNamed)
import Hedger.Infrastructure.SnapshotStore (
    SnapshotStore,
    SnapshotStoreRead (..),
    newSnapshotStore,
    readCurrentSnapshot,
    readSnapshotStore,
    reloadSnapshot,
 )
import Hedger.Interface.HTTP.API (application)
import Hedger.Interface.HTTP.Catalog qualified as CatalogHttp
import Hedger.Interface.HTTP.Core.Middleware (requestContextMiddleware)

makeApplication :: Config -> SnapshotStore -> Application
makeApplication configuration store = productionMiddleware (coreApplication configuration store)

type LedgerRegistry = Map LedgerId (LedgerConfig, SnapshotStore)

makeCatalogApplication :: Catalog -> LedgerRegistry -> Application
makeCatalogApplication catalog registry =
    productionMiddleware (CatalogHttp.catalogApplication catalog.defaultLedger handlers)
  where
    handlers = CatalogHttp.CatalogHandlers readReady readEntries statusFor appFor
    readReady = do
        let stores =
                case catalog.defaultLedger of
                    Nothing -> snd <$> Map.elems registry
                    Just ledgerId -> maybe [] (pure . snd) (Map.lookup ledgerId registry)
        snapshots <- traverse readCurrentSnapshot stores
        pure (any (maybe False (const True)) snapshots)
    readEntries =
        traverse
            readEntry
            [ (ledgerId, entry)
            | ledgerId <- catalog.ledgerOrder
            , Just entry <- [Map.lookup ledgerId registry]
            ]
    readEntry (ledgerId, (ledger, store)) = do
        state <- readSnapshotStore store
        pure (CatalogHttp.CatalogEntry ledgerId ledger.ledgerLabel (statusText state))
    statusFor ledgerId =
        fmap (fmap statusText . readSnapshotStore . snd) (Map.lookup ledgerId registry)
    appFor ledgerId =
        fmap
            (\(ledger, store) -> ledgerApplication ledgerId ledger.ledgerConfig store)
            (Map.lookup ledgerId registry)

statusText :: SnapshotStoreRead -> Text
statusText state =
    case state of
        SnapshotNotLoaded -> "unavailable"
        SnapshotLoadFailed _ -> "unavailable"
        SnapshotFresh _ -> "fresh"
        SnapshotStale _ _ -> "stale"

productionMiddleware :: Middleware
productionMiddleware = requestContextMiddleware

coreApplication :: Config -> SnapshotStore -> Application
coreApplication configuration store =
    application Env{config = configuration, snapshotStore = store, ledgerId = "default"}

ledgerApplication :: LedgerId -> Config -> SnapshotStore -> Application
ledgerApplication selectedId configuration store =
    application
        Env{config = configuration, snapshotStore = store, ledgerId = ledgerIdText selectedId}

runServer :: Catalog -> IO ()
runServer catalog = do
    registry <- traverse newLedgerStore catalog.ledgers
    mapM_ loadInitial (Map.toList registry)
    let reloadAll = mapM_ (void . reloadSnapshot . snd) (Map.elems registry)
    void (installHandler sigHUP (Catch reloadAll) Nothing)
    let settings =
            setInstallShutdownHandler installShutdownHandler
                . setBeforeMainLoop
                    ( Text.putStrLn
                        ( "\nStarted server on port "
                            <> Text.pack (show (portValue catalog.listenPort))
                        )
                    )
                . setGracefulShutdownTimeout (Just 10)
                . setHost (fromString (Text.unpack catalog.listenAddress))
                . setPort (portValue catalog.listenPort)
                $ defaultSettings
    let watcherStores =
            [(ledgerIdText ledgerId, store) | (ledgerId, (_, store)) <- Map.toList registry]
    withAllWatchers watcherStores $
        runSettings settings (makeCatalogApplication catalog registry)
  where
    loadInitial (ledgerId, (_, store)) = do
        result <- reloadSnapshot store
        Text.putStrLn $
            "Ledger "
                <> ledgerIdText ledgerId
                <> ": "
                <> case result of
                    Right _ -> "loaded"
                    Left problem -> "initial load failed: " <> Text.pack (show problem)

newLedgerStore :: LedgerConfig -> IO (LedgerConfig, SnapshotStore)
newLedgerStore ledger = do
    store <- newSnapshotStore ledger.ledgerConfig.journalPath
    pure (ledger, store)

withAllWatchers :: [(Text, SnapshotStore)] -> IO a -> IO a
withAllWatchers stores action =
    foldr (\(label, store) next -> withJournalWatcherNamed label store next) action stores

installShutdownHandler :: IO () -> IO ()
installShutdownHandler closeSocket = void (installHandler sigTERM (Catch closeSocket) Nothing)
