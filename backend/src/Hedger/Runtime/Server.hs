module Hedger.Runtime.Server (
    makeApplication,
    productionMiddleware,
    runServer,
    snapshotAvailable,
)
where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad (void)
import Data.Foldable (traverse_)
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
import Hedger.Infrastructure.DerivedRevision (DerivedState, newDerivedState)
import Hedger.Infrastructure.DerivedStore (DerivedStore, newDerivedStore)
import Hedger.Infrastructure.JournalWatcher (
    DerivedTargets (..),
    rebuildAfterReload,
    withJournalWatcher,
 )
import Hedger.Infrastructure.SnapshotStore (
    SnapshotStore,
    loadSnapshot,
    newSnapshotStore,
    reloadSnapshot,
 )
import Hedger.Interface.HTTP.API (application)
import Hedger.Interface.HTTP.Middleware (redactQueryForLogging, requestContextMiddleware)

makeApplication :: Config -> SnapshotStore -> DerivedStore -> DerivedState -> Application
makeApplication configuration store derived state =
    productionMiddleware id (coreApplication configuration store derived state)

productionMiddleware :: Middleware -> Middleware
productionMiddleware logger = requestContextMiddleware . redactQueryForLogging logger

coreApplication :: Config -> SnapshotStore -> DerivedStore -> DerivedState -> Application
coreApplication configuration store derived state =
    application
        Env
            { config = configuration
            , snapshotStore = store
            , derivedStore = derived
            , derivedState = state
            }

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
    state <- newDerivedState
    -- The boot load and SIGHUP publish a snapshot outside the watcher's reload
    -- cycle, so they must run the same rebuild; otherwise the derived view stays
    -- empty and every response reports "refreshing" until the first file event.
    let targets = DerivedTargets{derivedState = state, derivedStore = derived, config = configuration}
        reloadAndRebuild = do
            neverSuperseded <- newTVarIO False
            reloadSnapshot store configuration.journalPath
                >>= traverse_ (rebuildAfterReload targets neverSuperseded)
    reloadAndRebuild
    void (installHandler sigHUP (Catch reloadAndRebuild) Nothing)
    let settings =
            setInstallShutdownHandler installShutdownHandler
                . setBeforeMainLoop
                    (Text.putStrLn ("\nStarted server on port " <> Text.pack (show configuration.listenPort)))
                . setGracefulShutdownTimeout (Just 10)
                . setHost (fromString (Text.unpack configuration.listenAddress))
                . setPort configuration.listenPort
                $ defaultSettings
    withJournalWatcher store targets configuration.journalPath $
        runSettings
            settings
            ( productionMiddleware
                RequestLogger.logStdout
                (coreApplication configuration store derived state)
            )

installShutdownHandler :: IO () -> IO ()
installShutdownHandler closeSocket = void (installHandler sigTERM (Catch closeSocket) Nothing)

snapshotAvailable :: Text -> IO Bool
snapshotAvailable path = either (const False) (const True) <$> loadSnapshot path
