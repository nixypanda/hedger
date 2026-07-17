module Hedger.Runtime.Server (
    makeApplication,
    runServer,
    snapshotAvailable,
)
where

import Control.Monad (void)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Hedger.Application (Env (..))
import Hedger.Config (Config (..))
import Hedger.Infrastructure.Hledger.SnapshotStore (
    SnapshotStore,
    loadSnapshot,
    newSnapshotStore,
    reloadSnapshot,
 )
import Hedger.Interface.HTTP.API (application)
import Hedger.Interface.HTTP.Middleware (redactQueryForLogging, requestContextMiddleware)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (
    defaultSettings,
    runSettings,
    setGracefulShutdownTimeout,
    setHost,
    setInstallShutdownHandler,
    setPort,
 )
import Network.Wai.Middleware.RequestLogger qualified as RequestLogger
import System.Posix.Signals (Handler (Catch), installHandler, sigHUP, sigTERM)

makeApplication :: Config -> SnapshotStore -> Application
makeApplication configuration store =
    requestContextMiddleware (coreApplication configuration store)

coreApplication :: Config -> SnapshotStore -> Application
coreApplication configuration store =
    application Env{config = configuration, snapshotStore = store}

runServer :: Config -> IO ()
runServer configuration = do
    store <- newSnapshotStore
    void (reloadSnapshot store configuration.journalPath)
    void (installHandler sigHUP (Catch (void (reloadSnapshot store configuration.journalPath))) Nothing)
    let settings =
            setInstallShutdownHandler installShutdownHandler
                . setGracefulShutdownTimeout (Just 10)
                . setHost (fromString (Text.unpack configuration.listenAddress))
                . setPort configuration.listenPort
                $ defaultSettings
    runSettings
        settings
        ( requestContextMiddleware
            (redactQueryForLogging RequestLogger.logStdout (coreApplication configuration store))
        )

installShutdownHandler :: IO () -> IO ()
installShutdownHandler closeSocket = void (installHandler sigTERM (Catch closeSocket) Nothing)

snapshotAvailable :: Text -> IO Bool
snapshotAvailable path = either (const False) (const True) <$> loadSnapshot path
