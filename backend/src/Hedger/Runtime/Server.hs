module Hedger.Runtime.Server (
    makeApplication,
    productionMiddleware,
    runServer,
    snapshotAvailable,
)
where

import Control.Monad (void)
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
import Network.Wai.Middleware.RequestLogger qualified as RequestLogger
import System.Posix.Signals (Handler (Catch), installHandler, sigHUP, sigTERM)

import Hedger.Application.AppM (Env (..))
import Hedger.Config (Config (..))
import Hedger.Infrastructure.DerivedStore (DerivedStore, newDerivedStore)
import Hedger.Infrastructure.JournalWatcher (withJournalWatcher)
import Hedger.Infrastructure.SnapshotStore (
    SnapshotStore,
    loadSnapshot,
    newSnapshotStore,
    reloadSnapshot,
 )
import Hedger.Interface.HTTP.API (application)
import Hedger.Interface.HTTP.Middleware (redactQueryForLogging, requestContextMiddleware)

makeApplication :: Config -> SnapshotStore -> DerivedStore -> Application
makeApplication configuration store derived =
    productionMiddleware id (coreApplication configuration store derived)

productionMiddleware :: Middleware -> Middleware
productionMiddleware logger = requestContextMiddleware . redactQueryForLogging logger

coreApplication :: Config -> SnapshotStore -> DerivedStore -> Application
coreApplication configuration store derived =
    application Env{config = configuration, snapshotStore = store, derivedStore = derived}

{- | 64 MiB: a handful of megabytes per cached analytics response, so this holds
every family and dimension a session realistically touches while staying an
order of magnitude below the parsed journal snapshot's own footprint.
-}
derivedStoreBudgetBytes :: Int
derivedStoreBudgetBytes = 64 * 1024 * 1024

runServer :: Config -> IO ()
runServer configuration = do
    store <- newSnapshotStore
    derived <- newDerivedStore derivedStoreBudgetBytes
    void (reloadSnapshot store configuration.journalPath)
    void (installHandler sigHUP (Catch (void (reloadSnapshot store configuration.journalPath))) Nothing)
    let settings =
            setInstallShutdownHandler installShutdownHandler
                . setBeforeMainLoop
                    (Text.putStrLn ("\nStarted server on port " <> Text.pack (show configuration.listenPort)))
                . setGracefulShutdownTimeout (Just 10)
                . setHost (fromString (Text.unpack configuration.listenAddress))
                . setPort configuration.listenPort
                $ defaultSettings
    withJournalWatcher store configuration.journalPath $
        runSettings
            settings
            (productionMiddleware RequestLogger.logStdout (coreApplication configuration store derived))

installShutdownHandler :: IO () -> IO ()
installShutdownHandler closeSocket = void (installHandler sigTERM (Catch closeSocket) Nothing)

snapshotAvailable :: Text -> IO Bool
snapshotAvailable path = either (const False) (const True) <$> loadSnapshot path
