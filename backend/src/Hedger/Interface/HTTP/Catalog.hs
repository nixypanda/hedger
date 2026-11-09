module Hedger.Interface.HTTP.Catalog (
    CatalogHandlers (..),
    CatalogEntry (..),
    catalogApplication,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Network.HTTP.Types qualified as HTTP
import Network.Wai (Application)
import Network.Wai qualified as Wai

import Hedger.Config (LedgerId, ledgerIdText, mkLedgerId)
import Hedger.Interface.HTTP.Core.Errors (ErrorEnvelope (..), ErrorPayload (..))

data CatalogEntry = CatalogEntry
    { ledgerId :: LedgerId
    , label :: Text
    , status :: Text
    }

data CatalogHandlers = CatalogHandlers
    { ready :: IO Bool
    , entries :: IO [CatalogEntry]
    , ledgerStatus :: LedgerId -> Maybe (IO Text)
    , ledgerApp :: LedgerId -> Maybe Application
    }

catalogApplication :: Maybe LedgerId -> CatalogHandlers -> Application
catalogApplication defaultLedger handlers request respond =
    case Wai.pathInfo request of
        ["api", "v1", "health", "live"] ->
            getOnly $ respond (jsonResponse HTTP.status200 liveJson)
        ["api", "v1", "health", "ready"] ->
            getOnly $ do
                available <- handlers.ready
                let status = if available then HTTP.status200 else HTTP.status503
                    label = if available then "ok" else "unavailable" :: Text
                respond (jsonResponse status (Aeson.object ["status" Aeson..= label]))
        ["api", "v1", "ledgers"] ->
            getOnly $ do
                ledgers <- handlers.entries
                respond (jsonResponse HTTP.status200 (catalogJson defaultLedger ledgers))
        ["api", "v1", "ledgers", rawId, "status"] ->
            getOnly $ withLedger rawId $ \ledgerId ->
                case handlers.ledgerStatus ledgerId of
                    Nothing -> notFound
                    Just readStatus -> do
                        status <- readStatus
                        let body = Aeson.object ["status" Aeson..= status]
                        respond (jsonResponse HTTP.status200 body)
        "api" : "v1" : "ledgers" : rawId : rest ->
            withLedger rawId $ \ledgerId ->
                case handlers.ledgerApp ledgerId of
                    Nothing -> notFound
                    Just app ->
                        let path = ["api", "v1"] ++ rest
                            rawSegments =
                                ByteString.split 47 (ByteString.drop 1 request.rawPathInfo)
                            rawPath =
                                "/"
                                    <> ByteString.intercalate
                                        "/"
                                        (take 2 rawSegments ++ drop 4 rawSegments)
                            scopedRequest = request{Wai.pathInfo = path, Wai.rawPathInfo = rawPath}
                         in app scopedRequest respond
        _ -> errorResponse HTTP.status404 "not_found" "The API route was not found."
  where
    liveJson = Aeson.object ["status" Aeson..= ("ok" :: Text)]
    requestId =
        maybe "request-id-missing" TextEncoding.decodeUtf8 $
            lookup "X-Request-ID" request.requestHeaders
    getOnly action
        | request.requestMethod == HTTP.methodGet = action
        | otherwise =
            respond $
                Wai.responseLBS
                    HTTP.status405
                    [ (HTTP.hContentType, "application/json; charset=utf-8")
                    , ("Allow", "GET")
                    ]
                    ( Aeson.encode
                        (errorEnvelope requestId "method_not_allowed" "Use GET for this route.")
                    )
    withLedger rawId action =
        case mkLedgerId rawId of
            Left _ -> notFound
            Right ledgerId -> action ledgerId
    notFound = errorResponse HTTP.status404 "ledger_not_found" "Ledger not found."
    errorResponse status code message =
        respond (jsonResponse status (Aeson.toJSON (errorEnvelope requestId code message)))

catalogJson :: Maybe LedgerId -> [CatalogEntry] -> Aeson.Value
catalogJson defaultLedger entries =
    Aeson.object
        [ "defaultLedger" Aeson..= fmap ledgerIdText defaultLedger
        , "ledgers" Aeson..= (ledgerJson <$> entries)
        ]
  where
    ledgerJson entry =
        Aeson.object
            [ "id" Aeson..= ledgerIdText entry.ledgerId
            , "label" Aeson..= entry.label
            , "status" Aeson..= entry.status
            ]

jsonResponse :: HTTP.Status -> Aeson.Value -> Wai.Response
jsonResponse status body =
    Wai.responseLBS
        status
        [(HTTP.hContentType, "application/json; charset=utf-8")]
        (Aeson.encode body)

errorEnvelope :: Text -> Text -> Text -> ErrorEnvelope
errorEnvelope requestId code message =
    ErrorEnvelope
        { error =
            ErrorPayload
                { code = code
                , message = message
                , requestId = requestId
                , details = Nothing
                }
        }
