module Hedger.Domain.Period (
    -- * Types
    Granularity (..),
    ReportPeriod (..),

    -- * Calendar anchors and keys
    epoch,
    monthStart,
    monthKey,
    dayKey,
    monthPeriod,
    parseYearMonth,

    -- * List helpers
    takeLast,
    maximumMaybe,
    minimumMaybe,
    lastMaybe,
    atIndex,
)
where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, addDays, addGregorianMonthsClip, fromGregorian, toGregorian)
import Text.Read (readMaybe)

-- | The reporting cadence shared by the analytics and investment timelines.
data Granularity = Monthly | Yearly
    deriving stock (Eq, Show)

-- | An inclusive reporting window identified by a stable textual key.
data ReportPeriod = ReportPeriod
    { key :: Text
    , start :: Day
    , end :: Day
    }

-- | The reference date used when a journal contributes no dates of its own.
epoch :: Day
epoch = fromGregorian 1970 1 1

monthStart :: Day -> Day
monthStart day =
    let (year, month, _) = toGregorian day
     in fromGregorian year month 1

monthKey :: Day -> Text
monthKey day =
    let (year, month, _) = toGregorian day
     in Text.pack (show year <> "-" <> pad2 month)

dayKey :: Day -> Text
dayKey = Text.pack . show

monthPeriod :: Day -> ReportPeriod
monthPeriod day =
    let first = monthStart day
        next = addGregorianMonthsClip 1 first
     in ReportPeriod{key = monthKey first, start = first, end = addDays (-1) next}

parseYearMonth :: Text -> Maybe (Integer, Int)
parseYearMonth raw =
    case Text.splitOn "-" raw of
        [yearText, monthText] -> do
            year <- readText yearText
            month <- readText monthText
            if month >= 1 && month <= 12 && raw == Text.pack (show year <> "-" <> pad2 month)
                then Just (year, month)
                else Nothing
        _ -> Nothing
  where
    readText :: (Read value) => Text -> Maybe value
    readText = readMaybe . Text.unpack

pad2 :: Int -> String
pad2 value = if value < 10 then "0" <> show value else show value

takeLast :: Int -> [value] -> [value]
takeLast count values = drop (max 0 (length values - count)) values

maximumMaybe :: (Ord value) => [value] -> Maybe value
maximumMaybe [] = Nothing
maximumMaybe values = Just (maximum values)

minimumMaybe :: (Ord value) => [value] -> Maybe value
minimumMaybe [] = Nothing
minimumMaybe values = Just (minimum values)

lastMaybe :: [value] -> Maybe value
lastMaybe [] = Nothing
lastMaybe values = Just (last values)

atIndex :: Int -> [value] -> Maybe value
atIndex index values
    | index < 0 = Nothing
    | otherwise = case drop index values of
        value : _ -> Just value
        [] -> Nothing
