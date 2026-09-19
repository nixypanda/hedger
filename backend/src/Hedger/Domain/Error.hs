module Hedger.Domain.Error (
    AppError (..),
) where

import Data.Text (Text)

data AppError
    = JournalUnavailable
    | InvalidRequest Text
    | InvalidQuery Text (Maybe Int)
    | UnsupportedQueryOption Text
    | InvalidCursor Text
    | InvalidLimit Int Int
    | InvalidDate
    | InvalidDateRange
    | QueryTooLong Int
    | InternalError
    deriving stock (Eq, Show)
