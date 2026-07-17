module Hedger.Interface.HTTP.Middleware (
    redactQueryForLogging,
    redactQueryString,
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
import Hedger.Domain.Error (AppError (..))
import Hedger.Interface.HTTP.Errors (appErrorStatus, toEnvelope)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import System.IO.Unsafe (unsafePerformIO)

requestContextMiddleware :: Wai.Middleware
requestContextMiddleware app request respond = do
    requestId <- chooseRequestId (lookup "X-Request-ID" request.requestHeaders)
    let requestWithId = request{Wai.requestHeaders = ("X-Request-ID", Text.encodeUtf8 requestId) : request.requestHeaders}
    app requestWithId $ \response ->
        respond
            . Wai.mapResponseHeaders (("X-Request-ID", Text.encodeUtf8 requestId) :)
            $ normalizeServantError requestId response

{- | Run an access logger with a redacted request while the application receives
the original request. This keeps the hledger query out of logs without
changing request semantics.
-}
redactQueryForLogging :: Wai.Middleware -> Wai.Middleware
redactQueryForLogging logger app originalRequest respond =
    logger (\_ -> app originalRequest) redactedRequest respond
  where
    redactedRequest = originalRequest{Wai.rawQueryString = redactQueryString originalRequest.rawQueryString}

redactQueryString :: ByteString.ByteString -> ByteString.ByteString
redactQueryString raw =
    HTTP.renderQuery True (redact <$> HTTP.parseQuery raw)
  where
    redact (name, value)
        | name == "query" = (name, Just "[REDACTED]")
        | otherwise = (name, value)

normalizeServantError :: Text -> Wai.Response -> Wai.Response
normalizeServantError requestId response
    | HTTP.statusCode status == 404 && not (isJson response) = envelope404
    | HTTP.statusCode status == 405 && not (isJson response) = envelope405
    | HTTP.statusCode status == 400 && not (isJson response) = envelope (InvalidRequest "The request could not be decoded.")
    | HTTP.statusCode status >= 500 && not (isJson response) = envelope InternalError
    | otherwise = response
  where
    status = Wai.responseStatus response
    envelope appError = Wai.responseLBS (appErrorStatus appError) [(HTTP.hContentType, "application/json; charset=utf-8")] (Aeson.encode (toEnvelope requestId appError))
    envelope404 = Wai.responseLBS HTTP.status404 [(HTTP.hContentType, "application/json; charset=utf-8")] (Aeson.encode (routeEnvelope "not_found" "The requested API route does not exist."))
    envelope405 = Wai.responseLBS HTTP.status405 [(HTTP.hContentType, "application/json; charset=utf-8")] (Aeson.encode (routeEnvelope "method_not_allowed" "The requested method is not supported for this route."))
    routeEnvelope :: Text -> Text -> Aeson.Value
    routeEnvelope code message = Aeson.object ["error" Aeson..= Aeson.object ["code" Aeson..= code, "message" Aeson..= message, "requestId" Aeson..= requestId]]

isJson :: Wai.Response -> Bool
isJson response = maybe False (ByteString.isPrefixOf "application/json") (lookup HTTP.hContentType (Wai.responseHeaders response))

chooseRequestId :: Maybe ByteString.ByteString -> IO Text
chooseRequestId supplied = maybe nextRequestId pure (supplied >>= validRequestId)

validRequestId :: ByteString.ByteString -> Maybe Text
validRequestId raw
    | not (ByteString.null raw) && ByteString.length raw <= 128 && ByteString.all validByte raw = Just (Text.decodeUtf8 raw)
    | otherwise = Nothing
  where
    validByte byte = (byte >= 48 && byte <= 57) || (byte >= 65 && byte <= 90) || (byte >= 97 && byte <= 122) || byte `elem` [45, 95, 46, 58]

nextRequestId :: IO Text
nextRequestId = do
    value <- atomicModifyIORef' requestCounter (\current -> let next = current + 1 in (next, next))
    pure . LazyText.toStrict . TextBuilder.toLazyText $ "req-" <> TextBuilder.decimal value

requestCounter :: IORef Integer
requestCounter = unsafePerformIO (newIORef 0)
{-# NOINLINE requestCounter #-}
