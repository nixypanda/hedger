module Hedger.Domain.Error (
    AppError (..),
    ErrorEnvelope (..),
    ErrorPayload (..),
    appErrorStatus,
    toEnvelope,
)
where

import Data.Aeson qualified as Aeson
import Data.Aeson.TH qualified as Aeson
import Data.Text (Text)
import Network.HTTP.Types qualified as HTTP

data AppError
    = JournalUnavailable
    | InvalidRequest Text
    | RouteNotFound
    | MethodNotAllowed
    | InternalError
    deriving (Eq, Show)

data ErrorPayload = ErrorPayload
    { code :: Text
    , message :: Text
    , requestId :: Text
    }
    deriving (Eq, Show)

Aeson.deriveJSON Aeson.defaultOptions ''ErrorPayload

data ErrorEnvelope = ErrorEnvelope
    { errorPayload :: ErrorPayload
    }
    deriving (Eq, Show)

Aeson.deriveJSON Aeson.defaultOptions{Aeson.fieldLabelModifier = const "error"} ''ErrorEnvelope

appErrorStatus :: AppError -> HTTP.Status
appErrorStatus JournalUnavailable = HTTP.status503
appErrorStatus (InvalidRequest _) = HTTP.status400
appErrorStatus RouteNotFound = HTTP.status404
appErrorStatus MethodNotAllowed = HTTP.status405
appErrorStatus InternalError = HTTP.status500

toEnvelope :: Text -> AppError -> ErrorEnvelope
toEnvelope requestIdentifier appError =
    ErrorEnvelope
        { errorPayload =
            ErrorPayload
                { code = errorCode appError
                , message = errorMessage appError
                , requestId = requestIdentifier
                }
        }
  where
    errorCode JournalUnavailable = "journal_unavailable"
    errorCode (InvalidRequest _) = "invalid_request"
    errorCode RouteNotFound = "not_found"
    errorCode MethodNotAllowed = "method_not_allowed"
    errorCode InternalError = "internal_error"

    errorMessage JournalUnavailable = "No usable journal snapshot is available."
    errorMessage (InvalidRequest detail) = detail
    errorMessage RouteNotFound = "The requested API route does not exist."
    errorMessage MethodNotAllowed = "The requested method is not supported for this route."
    errorMessage InternalError = "An unexpected server error occurred."
