module Hedger.Runtime.RequestContext (
    requestContextMiddleware,
)
where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder qualified as TextBuilder
import Data.Text.Lazy.Builder.Int qualified as TextBuilder
import Hedger.Domain.Error qualified as AppError
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import System.IO.Unsafe (unsafePerformIO)

-- This middleware is API plumbing, not logging. It makes the request ID used
-- by stable error envelopes available to Servant and returns it to the client.
requestContextMiddleware :: Wai.Middleware
requestContextMiddleware app request respond = do
    requestId <- chooseRequestId (lookup "X-Request-ID" request.requestHeaders)
    let requestWithId = request{Wai.requestHeaders = ("X-Request-ID", Text.encodeUtf8 requestId) : request.requestHeaders}
    app requestWithId $ \response ->
        respond
            . Wai.mapResponseHeaders (("X-Request-ID", Text.encodeUtf8 requestId) :)
            $ normalizeServantError requestId response

normalizeServantError :: Text -> Wai.Response -> Wai.Response
normalizeServantError requestId response
    | HTTP.statusCode status == 404 && not (isJson response) = envelope AppError.RouteNotFound
    | HTTP.statusCode status == 405 && not (isJson response) = envelope AppError.MethodNotAllowed
    | HTTP.statusCode status == 400 && not (isJson response) = envelope (AppError.InvalidRequest "The request could not be decoded.")
    | HTTP.statusCode status >= 500 && not (isJson response) = envelope AppError.InternalError
    | otherwise = response
  where
    status = Wai.responseStatus response
    envelope appError =
        Wai.responseLBS
            (AppError.appErrorStatus appError)
            [(HTTP.hContentType, "application/json; charset=utf-8")]
            (Aeson.encode (AppError.toEnvelope requestId appError))

isJson :: Wai.Response -> Bool
isJson response =
    maybe False (ByteString.isPrefixOf "application/json") (lookup HTTP.hContentType (Wai.responseHeaders response))

chooseRequestId :: Maybe ByteString.ByteString -> IO Text
chooseRequestId supplied =
    case supplied >>= validRequestId of
        Just requestId -> pure requestId
        Nothing -> nextRequestId

validRequestId :: ByteString.ByteString -> Maybe Text
validRequestId raw
    | not (ByteString.null raw) && ByteString.length raw <= 128 && ByteString.all validByte raw = Just (Text.decodeUtf8 raw)
    | otherwise = Nothing
  where
    validByte byte =
        (byte >= 48 && byte <= 57)
            || (byte >= 65 && byte <= 90)
            || (byte >= 97 && byte <= 122)
            || byte `elem` [45, 95, 46, 58]

nextRequestId :: IO Text
nextRequestId = do
    value <- atomicModifyIORef' requestCounter (\current -> let next = current + 1 in (next, next))
    pure . LazyText.toStrict . TextBuilder.toLazyText $ "req-" <> TextBuilder.decimal value

requestCounter :: IORef Integer
requestCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE requestCounter #-}
