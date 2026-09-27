module Hedger.Domain.Period (
    -- * Types
    Granularity (..),
    MonthOfYear,
    mkMonthOfYear,
    monthOfYearValue,
    ReportPeriod,
    reportPeriodKey,
    reportPeriodStart,
    reportPeriodEnd,

    -- * Calendar anchors and keys
    epoch,
    monthStart,
    monthKey,
    dayKey,
    monthPeriod,
    fiscalYearPeriod,
    periodsFromDateSpan,
    periodsFromReport,
    YearMonth,
    parseYearMonth,
    yearMonthParts,
    yearMonthStart,
    yearMonthMonth,
    intervalFor,

    -- * List helpers
    takeLast,
    maximumMaybe,
    minimumMaybe,
    lastMaybe,
    atIndex,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, addDays, fromGregorian, toGregorian)
import Numeric.Natural (Natural)
import Text.Read (readMaybe)

import Hledger.Data.DayPartition qualified as HledgerDayPartition
import Hledger.Data.Types qualified as Hledger
import Hledger.Reports.ReportOptions qualified as HledgerReportOptions

-- | The reporting cadence shared by the analytics and investment timelines.
data Granularity
    = Monthly
    | Yearly
    deriving stock (Eq, Show)

-- | A calendar month in @[1, 12]@.
newtype MonthOfYear = MonthOfYear Int
    deriving stock (Eq, Show)

mkMonthOfYear :: Int -> Maybe MonthOfYear
mkMonthOfYear value
    | value >= 1 && value <= 12 = Just (MonthOfYear value)
    | otherwise = Nothing

monthOfYearValue :: MonthOfYear -> Int
monthOfYearValue (MonthOfYear value) = value

-- | An inclusive reporting window identified by a stable textual key.
data ReportPeriod = ReportPeriod
    { key :: Text
    , start :: Day
    , end :: Day
    }

{- | Build a period whose interval is ordered: an end before the start is
clamped up to the start, so @start <= end@ holds for every value.
-}
mkReportPeriod :: Text -> Day -> Day -> ReportPeriod
mkReportPeriod key start end = ReportPeriod{key, start, end = max start end}

reportPeriodKey :: ReportPeriod -> Text
reportPeriodKey period = period.key

reportPeriodStart :: ReportPeriod -> Day
reportPeriodStart period = period.start

reportPeriodEnd :: ReportPeriod -> Day
reportPeriodEnd period = period.end

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

fiscalYearPeriod :: MonthOfYear -> Day -> ReportPeriod
fiscalYearPeriod startMonth =
    periodContaining (Hledger.MonthAndDay (monthOfYearValue startMonth) 1) monthKey

periodsFromDateSpan :: Granularity -> Hledger.DateSpan -> [ReportPeriod]
periodsFromDateSpan granularity dateSpan =
    maybe
        []
        (fmap (uncurry (reportPeriod granularity)) . HledgerDayPartition.dayPartitionToList)
        (HledgerDayPartition.splitSpan True (intervalFor granularity) dateSpan)

periodsFromReport ::
    Granularity -> Hledger.Journal -> HledgerReportOptions.ReportSpec -> [ReportPeriod]
periodsFromReport granularity journal reportSpec =
    maybe
        []
        (fmap (uncurry (reportPeriod granularity)) . HledgerDayPartition.dayPartitionToList)
        partition
  where
    options = reportSpec._rsReportOpts{HledgerReportOptions.interval_ = intervalFor granularity}
    (_, partition) =
        HledgerReportOptions.reportSpan
            journal
            reportSpec{HledgerReportOptions._rsReportOpts = options}

periodContaining :: Hledger.Interval -> (Day -> Text) -> Day -> ReportPeriod
periodContaining interval keyFor day =
    case HledgerDayPartition.splitSpan True interval enclosingSpan of
        Just partition ->
            let (start, end) = HledgerDayPartition.dayPartitionFind day partition
                periodStart = maybe day id start
             in mkReportPeriod (keyFor periodStart) periodStart end
        Nothing -> mkReportPeriod (keyFor day) day day
  where
    enclosingSpan =
        Hledger.DateSpan (Just (Hledger.Exact day)) (Just (Hledger.Exact (addDays 1 day)))

intervalFor :: Granularity -> Hledger.Interval
intervalFor Monthly = Hledger.Months 1
intervalFor Yearly = Hledger.Years 1

reportPeriod :: Granularity -> Day -> Day -> ReportPeriod
reportPeriod granularity start end =
    mkReportPeriod keyValue start end
  where
    keyValue =
        case granularity of
            Monthly -> monthKey start
            Yearly ->
                let (year, _, _) = toGregorian start
                 in Text.pack (show year)

{- | A validated @YYYY-MM@. The constructor is not exported, so the month is
always in @[1, 12]@ and the text form is canonical.
-}
newtype YearMonth = YearMonth (Integer, Int)
    deriving stock (Eq, Ord, Show)

yearMonthParts :: YearMonth -> (Integer, Int)
yearMonthParts (YearMonth parts) = parts

-- | The first day of the represented month.
yearMonthStart :: YearMonth -> Day
yearMonthStart (YearMonth (year, month)) = fromGregorian year month 1

yearMonthMonth :: YearMonth -> Int
yearMonthMonth (YearMonth (_, month)) = month

parseYearMonth :: Text -> Maybe YearMonth
parseYearMonth raw =
    case Text.splitOn "-" raw of
        [yearText, monthText] -> do
            year <- readText yearText
            month <- readText monthText
            if month >= 1 && month <= 12 && raw == Text.pack (show year <> "-" <> pad2 month)
                then Just (YearMonth (year, month))
                else Nothing
        _ -> Nothing
  where
    readText :: (Read value) => Text -> Maybe value
    readText = readMaybe . Text.unpack

pad2 :: Int -> String
pad2 value =
    if value < 10
        then "0" <> show value
        else show value

takeLast :: Natural -> [value] -> [value]
takeLast count values =
    let requested = fromIntegral count :: Int
     in drop (length values - min requested (length values)) values

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
    | otherwise =
        case drop index values of
            value : _ -> Just value
            [] -> Nothing
