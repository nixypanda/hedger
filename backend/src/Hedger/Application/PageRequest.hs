module Hedger.Application.PageRequest (
    PageLimit,
    mkPageLimit,
    pageLimitValue,
    defaultPageLimit,
    maximumPageLimit,
    DateRange,
    allDates,
    mkDateRange,
    dateRangeBounds,
) where

import Data.Time (Day)

import Hedger.Domain.Error (AppError (..))

newtype PageLimit = PageLimit Int
    deriving stock (Eq, Show)

mkPageLimit :: Int -> Either AppError PageLimit
mkPageLimit value
    | value >= 1 && value <= pageLimitValue maximumPageLimit = Right (PageLimit value)
    | otherwise =
        Left (InvalidLimit (pageLimitValue defaultPageLimit) (pageLimitValue maximumPageLimit))

pageLimitValue :: PageLimit -> Int
pageLimitValue (PageLimit value) = value

defaultPageLimit :: PageLimit
defaultPageLimit = PageLimit 50

maximumPageLimit :: PageLimit
maximumPageLimit = PageLimit 200

data DateRange
    = AllDates
    | StartingOn Day
    | EndingOn Day
    | Between Day Day
    deriving stock (Eq, Show)

allDates :: DateRange
allDates = AllDates

mkDateRange :: Maybe Day -> Maybe Day -> Either AppError DateRange
mkDateRange Nothing Nothing = Right AllDates
mkDateRange (Just start) Nothing = Right (StartingOn start)
mkDateRange Nothing (Just end) = Right (EndingOn end)
mkDateRange (Just start) (Just end)
    | start <= end = Right (Between start end)
    | otherwise = Left InvalidDateRange

dateRangeBounds :: DateRange -> (Maybe Day, Maybe Day)
dateRangeBounds AllDates = (Nothing, Nothing)
dateRangeBounds (StartingOn start) = (Just start, Nothing)
dateRangeBounds (EndingOn end) = (Nothing, Just end)
dateRangeBounds (Between start end) = (Just start, Just end)
