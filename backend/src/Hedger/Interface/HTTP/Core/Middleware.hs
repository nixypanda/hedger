module Hedger.Interface.HTTP.Core.Middleware (
    requestContextMiddleware,
)
where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai

import Hedger.Domain.Error (AppError (..))
import Hedger.Interface.HTTP.Core.Errors (appErrorStatus, toEnvelope)

requestContextMiddleware :: Wai.Middleware
requestContextMiddleware app request respond = do
    requestId <- chooseRequestId (lookup "X-Request-ID" request.requestHeaders)
    let requestWithId =
            request{Wai.requestHeaders = ("X-Request-ID", Text.encodeUtf8 requestId) : request.requestHeaders}
    app requestWithId $ \response ->
        respond
            . Wai.mapResponseHeaders (("X-Request-ID", Text.encodeUtf8 requestId) :)
            $ normalizeServantError requestId response

normalizeServantError :: Text -> Wai.Response -> Wai.Response
normalizeServantError requestId response
    | HTTP.statusCode status == 404 && not (isJson response) = envelope404
    | HTTP.statusCode status == 405 && not (isJson response) = envelope405
    | HTTP.statusCode status == 400 && not (isJson response) =
        envelope (InvalidRequest "The request could not be decoded.")
    | HTTP.statusCode status >= 500 && not (isJson response) = envelope InternalError
    | otherwise = response
  where
    status = Wai.responseStatus response
    envelope appError =
        Wai.responseLBS
            (appErrorStatus appError)
            [(HTTP.hContentType, "application/json; charset=utf-8")]
            (Aeson.encode (toEnvelope requestId appError))
    envelope404 =
        Wai.responseLBS
            HTTP.status404
            [(HTTP.hContentType, "application/json; charset=utf-8")]
            (Aeson.encode (routeEnvelope "not_found" "Requested API route does not exist."))
    envelope405 =
        Wai.responseLBS
            HTTP.status405
            [(HTTP.hContentType, "application/json; charset=utf-8")]
            (Aeson.encode (routeEnvelope "method_not_allowed" "Requested method is not supported for route."))
    routeEnvelope :: Text -> Text -> Aeson.Value
    routeEnvelope code message =
        Aeson.object
            [ "error"
                Aeson..= Aeson.object
                    ["code" Aeson..= code, "message" Aeson..= message, "requestId" Aeson..= requestId]
            ]

isJson :: Wai.Response -> Bool
isJson response =
    maybe
        False
        (ByteString.isPrefixOf "application/json")
        (lookup HTTP.hContentType (Wai.responseHeaders response))

chooseRequestId :: Maybe ByteString.ByteString -> IO Text
chooseRequestId supplied = maybe generateRequestId pure (fmap Text.decodeUtf8 (supplied >>= validRequestId))

validRequestId :: ByteString.ByteString -> Maybe ByteString.ByteString
validRequestId raw
    | not (ByteString.null raw) = Just raw
    | otherwise = Nothing

generateRequestId :: IO Text
generateRequestId = UUID.toText <$> UUID.nextRandom
