module Hedger.Interface.HTTP.Core.Errors (
    ErrorEnvelope (..),
    ErrorPayload (..),
    ValidationDetails (..),
    appErrorStatus,
    toEnvelope,
)
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Network.HTTP.Types qualified as HTTP

import Hedger.Domain.Error (AppError (..))

data ErrorPayload = ErrorPayload
    { code :: Text
    , message :: Text
    , requestId :: Text
    , details :: Maybe ValidationDetails
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ValidationDetails = ValidationDetails
    { reason :: Text
    , position :: Maybe Int
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ErrorEnvelope = ErrorEnvelope
    { error :: ErrorPayload
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

appErrorStatus :: AppError -> HTTP.Status
appErrorStatus JournalUnavailable = HTTP.status503
appErrorStatus (InvalidRequest _) = HTTP.status400
appErrorStatus (InvalidQuery _ _) = HTTP.status400
appErrorStatus (UnsupportedQueryOption _) = HTTP.status400
appErrorStatus (InvalidCursor _) = HTTP.status400
appErrorStatus (InvalidLimit _ _) = HTTP.status400
appErrorStatus InvalidDate = HTTP.status400
appErrorStatus InvalidDateRange = HTTP.status400
appErrorStatus (QueryTooLong _) = HTTP.status400
appErrorStatus InternalError = HTTP.status500

toEnvelope :: Text -> AppError -> ErrorEnvelope
toEnvelope requestIdentifier appError =
    ErrorEnvelope
        { error =
            ErrorPayload
                { code = errorCode appError
                , message = errorMessage appError
                , requestId = requestIdentifier
                , details = errorDetails appError
                }
        }
  where
    errorCode JournalUnavailable = "journal_unavailable"
    errorCode (InvalidRequest _) = "invalid_request"
    errorCode (InvalidQuery _ _) = "invalid_query"
    errorCode (UnsupportedQueryOption _) = "unsupported_query_option"
    errorCode (InvalidCursor _) = "invalid_cursor"
    errorCode (InvalidLimit _ _) = "invalid_limit"
    errorCode InvalidDate = "invalid_date"
    errorCode InvalidDateRange = "invalid_date_range"
    errorCode (QueryTooLong _) = "query_too_long"
    errorCode InternalError = "internal_error"

    errorMessage JournalUnavailable = "No usable journal snapshot is available."
    errorMessage (InvalidRequest detail) = detail
    errorMessage (InvalidQuery _ _) = "The transaction query is invalid."
    errorMessage (UnsupportedQueryOption _) = "The transaction query uses unsupported report options."
    errorMessage (InvalidCursor detail) = detail
    errorMessage (InvalidLimit lowerBound upperBound) =
        "The limit must be between " <> showText lowerBound <> " and " <> showText upperBound <> "."
    errorMessage InvalidDate = "Dates must use ISO-8601 YYYY-MM-DD format."
    errorMessage InvalidDateRange = "The from date must not be after the to date."
    errorMessage (QueryTooLong upperBound) =
        "The query must not exceed " <> showText upperBound <> " characters."
    errorMessage InternalError = "An unexpected server error occurred."

    errorDetails (InvalidQuery reason position) = Just ValidationDetails{reason = reason, position = position}
    errorDetails (UnsupportedQueryOption options) =
        Just ValidationDetails{reason = "Unsupported report option(s): " <> options, position = Nothing}
    errorDetails _ = Nothing

    showText = Text.pack . show
