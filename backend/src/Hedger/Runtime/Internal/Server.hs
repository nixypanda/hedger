module Hedger.Runtime.Internal.Server (
    makeApplication,
    productionMiddleware,
    runServer,
)
where

import Control.Monad (void)
import Data.String (fromString)
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
import Hedger.Infrastructure.JournalWatcher (withJournalWatcher)
import Hedger.Infrastructure.SnapshotStore (
    SnapshotStore,
    newSnapshotStore,
    reloadSnapshot,
 )
import Hedger.Interface.HTTP.API (application)
import Hedger.Interface.HTTP.Middleware (redactQueryForLogging, requestContextMiddleware)

makeApplication :: Config -> SnapshotStore -> Application
makeApplication configuration store =
    productionMiddleware id (coreApplication configuration store)

productionMiddleware :: Middleware -> Middleware
productionMiddleware logger = requestContextMiddleware . redactQueryForLogging logger

coreApplication :: Config -> SnapshotStore -> Application
coreApplication configuration store =
    application Env{config = configuration, snapshotStore = store}

runServer :: Config -> IO ()
runServer configuration = do
    store <- newSnapshotStore configuration.journalPath
    void (reloadSnapshot store)
    void (installHandler sigHUP (Catch (void (reloadSnapshot store))) Nothing)
    let settings =
            setInstallShutdownHandler installShutdownHandler
                . setBeforeMainLoop
                    (Text.putStrLn ("\nStarted server on port " <> Text.pack (show configuration.listenPort)))
                . setGracefulShutdownTimeout (Just 10)
                . setHost (fromString (Text.unpack configuration.listenAddress))
                . setPort configuration.listenPort
                $ defaultSettings
    withJournalWatcher store $
        runSettings
            settings
            (productionMiddleware RequestLogger.logStdout (coreApplication configuration store))

installShutdownHandler :: IO () -> IO ()
installShutdownHandler closeSocket = void (installHandler sigTERM (Catch closeSocket) Nothing)
