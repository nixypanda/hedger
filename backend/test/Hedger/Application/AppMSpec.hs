module Hedger.Application.AppMSpec (appMSpec) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad.IO.Class (liftIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

import Hedger.Application.AppM (AppM, Env (..), runAppM)
import Hedger.Domain.Error (AppError (InternalError))
import Hedger.Infrastructure.SnapshotStore (newSnapshotStore)
import Hedger.TestHelpers (configuration, testEnv)

appMSpec :: TestTree
appMSpec =
    testGroup
        "AppM"
        [ testCase
            "runAppM converts a synchronous exception from an action into InternalError"
            convertsSynchronousExceptionToInternalError
        ]

convertsSynchronousExceptionToInternalError :: IO ()
convertsSynchronousExceptionToInternalError = do
    store <- newSnapshotStore
    env <- testEnv store
    result <- try (runAppM env badAction) :: IO (Either SomeException (Either AppError Int))
    case result of
        Right (Left InternalError) -> pure ()
        other -> assertFailure ("expected Right (Left InternalError), got: " <> show other)

badAction :: AppM Int
badAction = liftIO (evaluate (error "boom" :: Int))
