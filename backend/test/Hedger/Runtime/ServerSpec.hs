module Hedger.Runtime.ServerSpec (serverSpec) where

import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (newIORef, readIORef, writeIORef)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Test (SResponse (..), defaultRequest, request, runSession, setPath)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Hedger.Runtime.Server (productionMiddleware)

serverSpec :: TestTree
serverSpec =
    testGroup
        "production server middleware"
        [testCase "redacts logging without changing the request and preserves ordering" compositionIsSafe]

compositionIsSafe :: IO ()
compositionIsSafe = do
    loggedRequest <- newIORef Nothing
    let logger downstream waiRequest respond = do
            writeIORef
                loggedRequest
                (Just (waiRequest.rawQueryString, lookup requestId waiRequest.requestHeaders))
            downstream waiRequest respond
        echoApp waiRequest respond =
            respond (Wai.responseLBS HTTP.status200 [] (LazyByteString.fromStrict waiRequest.rawQueryString))
        requestValue =
            (setPath defaultRequest "/v1/example?query=acct%3Asecret&limit=1")
                { Wai.requestHeaders = [(requestId, "middleware-test")]
                }
    response <- runSession (request requestValue) (productionMiddleware logger echoApp)
    observed <- readIORef loggedRequest
    assertEqual
        "logger should receive the redacted query after request context"
        (Just ("?query=%5BREDACTED%5D&limit=1", Just "middleware-test"))
        observed
    assertEqual
        "downstream app should receive the original query"
        "?query=acct%3Asecret&limit=1"
        response.simpleBody
    assertEqual
        "response should carry the same request ID"
        (Just "middleware-test")
        (lookup requestId response.simpleHeaders)
  where
    requestId = "X-Request-ID" :: HTTP.HeaderName
