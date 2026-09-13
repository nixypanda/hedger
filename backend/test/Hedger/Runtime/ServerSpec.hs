module Hedger.Runtime.ServerSpec (serverSpec) where

import Data.ByteString.Lazy qualified as LazyByteString
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Test (SResponse (..), defaultRequest, request, runSession, setPath)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Hedger.Interface.HTTP.Middleware (requestContextMiddleware)

serverSpec :: TestTree
serverSpec =
    testGroup
        "production server middleware"
        [ testCase "injects request ID into response headers" injectsRequestId
        , testCase "preserves supplied request ID" preservesSuppliedRequestId
        ]

injectsRequestId :: IO ()
injectsRequestId = do
    let echoApp waiRequest respond =
            respond (Wai.responseLBS HTTP.status200 [] (LazyByteString.fromStrict waiRequest.rawQueryString))
        requestValue = setPath defaultRequest "/v1/example?query=acct%3Asecret&limit=1"
    response <- runSession (request requestValue) (requestContextMiddleware echoApp)
    assertEqual
        "response should carry a generated request ID"
        True
        (any (\(name, _) -> name == "X-Request-ID") response.simpleHeaders)

preservesSuppliedRequestId :: IO ()
preservesSuppliedRequestId = do
    let echoApp waiRequest respond =
            respond (Wai.responseLBS HTTP.status200 [] (LazyByteString.fromStrict waiRequest.rawQueryString))
        requestValue =
            (setPath defaultRequest "/v1/example")
                { Wai.requestHeaders = [(requestId, "my-custom-id")]
                }
    response <- runSession (request requestValue) (requestContextMiddleware echoApp)
    assertEqual
        "response should carry the supplied request ID"
        (Just "my-custom-id")
        (lookup requestId response.simpleHeaders)
  where
    requestId = "X-Request-ID" :: HTTP.HeaderName
