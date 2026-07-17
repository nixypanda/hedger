module Hedger.Domain.Error (
    AppError (..),
)
where

import Data.Text (Text)

data AppError
    = JournalUnavailable
    | InvalidRequest Text
    | InternalError
    deriving (Eq, Show)
