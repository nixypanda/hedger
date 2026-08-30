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
    fiscalYearPeriod,
    periodsFromDateSpan,
    periodsFromReport,
    parseYearMonth,
    intervalFor,

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
import Data.Time (Day, addDays, fromGregorian, toGregorian)
import Text.Read (readMaybe)

import Hledger.Data.DayPartition qualified as HledgerDayPartition
import Hledger.Data.Types qualified as Hledger
import Hledger.Reports.ReportOptions qualified as HledgerReportOptions

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
monthStart = HledgerDayPartition.intervalBoundaryBefore (Hledger.Months 1)

monthKey :: Day -> Text
monthKey day =
    let (year, month, _) = toGregorian day
     in Text.pack (show year <> "-" <> pad2 month)

dayKey :: Day -> Text
dayKey = Text.pack . show

monthPeriod :: Day -> ReportPeriod
monthPeriod = periodContaining (Hledger.Months 1) monthKey

fiscalYearPeriod :: Int -> Day -> ReportPeriod
fiscalYearPeriod startMonth = periodContaining (Hledger.MonthAndDay startMonth 1) monthKey

periodsFromDateSpan :: Granularity -> Hledger.DateSpan -> [ReportPeriod]
periodsFromDateSpan granularity dateSpan =
    maybe
        []
        (fmap (uncurry (reportPeriod granularity)) . HledgerDayPartition.dayPartitionToList)
        (HledgerDayPartition.splitSpan True (intervalFor granularity) dateSpan)

periodsFromReport :: Granularity -> Hledger.Journal -> HledgerReportOptions.ReportSpec -> [ReportPeriod]
periodsFromReport granularity journal reportSpec =
    maybe
        []
        (fmap (uncurry (reportPeriod granularity)) . HledgerDayPartition.dayPartitionToList)
        partition
  where
    options =
        reportSpec._rsReportOpts
            { HledgerReportOptions.interval_ = intervalFor granularity
            }
    (_, partition) =
        HledgerReportOptions.reportSpan
            journal
            reportSpec{HledgerReportOptions._rsReportOpts = options}

periodContaining :: Hledger.Interval -> (Day -> Text) -> Day -> ReportPeriod
periodContaining interval keyFor day =
    case HledgerDayPartition.splitSpan True interval enclosingSpan of
        Just partition ->
            let (start, end) = HledgerDayPartition.dayPartitionFind day partition
             in ReportPeriod
                    { key = keyFor (maybe day id start)
                    , start = maybe day id start
                    , end
                    }
        Nothing -> ReportPeriod{key = keyFor day, start = day, end = day}
  where
    enclosingSpan = Hledger.DateSpan (Just (Hledger.Exact day)) (Just (Hledger.Exact (addDays 1 day)))

intervalFor :: Granularity -> Hledger.Interval
intervalFor Monthly = Hledger.Months 1
intervalFor Yearly = Hledger.Years 1

reportPeriod :: Granularity -> Day -> Day -> ReportPeriod
reportPeriod granularity start end =
    ReportPeriod
        { key = case granularity of
            Monthly -> monthKey start
            Yearly -> let (year, _, _) = toGregorian start in Text.pack (show year)
        , start
        , end
        }

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
