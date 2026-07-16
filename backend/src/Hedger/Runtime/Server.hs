module Hedger.Runtime.Server (
    makeApplication,
    runServer,
    snapshotAvailable,
)
where

import Control.Exception (IOException, bracket, try)
import Control.Monad (void)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Hedger.Api (application)
import Hedger.Application (Env (..))
import Hedger.Config (Config (..))
import Hedger.Runtime.RequestContext (requestContextMiddleware)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setGracefulShutdownTimeout, setHost, setInstallShutdownHandler, setPort)
import Network.Wai.Middleware.RequestLogger qualified as RequestLogger
import System.IO (IOMode (ReadMode), hClose, openFile)
import System.Posix.Signals (Handler (Catch), installHandler, sigTERM)

makeApplication :: Config -> Bool -> Application
makeApplication configuration available =
    requestContextMiddleware (coreApplication configuration available)

coreApplication :: Config -> Bool -> Application
coreApplication configuration available =
    application Env{config = configuration, hasUsableSnapshot = available}

runServer :: Config -> IO ()
runServer configuration = do
    available <- snapshotAvailable configuration.journalPath
    let settings =
            setInstallShutdownHandler installShutdownHandler
                . setGracefulShutdownTimeout (Just 10)
                . setHost (fromString (Text.unpack configuration.listenAddress))
                . setPort configuration.listenPort
                $ defaultSettings
    runSettings settings (requestContextMiddleware (RequestLogger.logStdout (coreApplication configuration available)))

installShutdownHandler :: IO () -> IO ()
installShutdownHandler closeSocket = void (installHandler sigTERM (Catch closeSocket) Nothing)

snapshotAvailable :: Text -> IO Bool
snapshotAvailable path = do
    opened <- try (bracket (openFile (Text.unpack path) ReadMode) hClose (const (pure ()))) :: IO (Either IOException ())
    pure (either (const False) (const True) opened)
