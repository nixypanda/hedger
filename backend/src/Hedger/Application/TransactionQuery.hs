module Hedger.Application.TransactionQuery (
    TransactionPageRequest (..),
    TransactionPage (..),
    transactionPage,
    parseHledgerQuery,
) where

import Data.Char (isDigit)
import Data.Function (on)
import Data.List qualified as List
import Data.Ord (Down (Down))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, fromGregorian)
import Text.Read (readMaybe)

import Hledger.Data.Types qualified as Hledger
import Hledger.Query qualified as Hledger
import Hledger.Reports.EntriesReport qualified as HledgerEntries
import Hledger.Reports.ReportOptions qualified as HledgerReportOptions

import Hedger.Application.Internal.Cursor (decodeTransactionCursor, encodeTransactionCursor)
import Hedger.Application.PageRequest (PageLimit, pageLimitValue)
import Hedger.Domain.Error (AppError (..))
import Hedger.Domain.Snapshot (Snapshot (..))

data TransactionPageRequest = TransactionPageRequest
    { query :: Maybe Text
    , limit :: PageLimit
    , cursor :: Maybe Text
    }

data TransactionPage = TransactionPage
    { transactions :: [Hledger.Transaction]
    , nextCursor :: Maybe Text
    }
    deriving stock (Show)

transactionPage :: Snapshot -> TransactionPageRequest -> Either AppError TransactionPage
transactionPage snapshot request = do
    let canonicalQuery = Text.strip (maybe "" id request.query)
        referenceDate = maybe emptyJournalReferenceDate id snapshot.latestJournalDate
    parsedQuery <- parseHledgerQuery referenceDate canonicalQuery
    offset <-
        maybe
            (Right 0)
            (decodeTransactionCursor snapshot.revision canonicalQuery referenceDate)
            request.cursor
    let reportSpec =
            HledgerReportOptions.defreportspec
                { HledgerReportOptions._rsDay = referenceDate
                , HledgerReportOptions._rsQuery = parsedQuery
                }
        journal = snapshot.journal{Hledger.jtxns = snapshot.transactions}
        matchingAscending = HledgerEntries.entriesReport reportSpec journal
        matching =
            concatMap
                (List.sortOn (Down . (.tindex)))
                (reverse (List.groupBy ((==) `on` (.tdate)) matchingAscending))
    if offset > length matching
        then Left (InvalidCursor "The cursor position is no longer valid.")
        else do
            let remaining = drop offset matching
                page = take (pageLimitValue request.limit) remaining
                followingOffset = offset + length page
                cursor =
                    if length remaining > pageLimitValue request.limit
                        then
                            Just
                                ( encodeTransactionCursor
                                    snapshot.revision
                                    canonicalQuery
                                    referenceDate
                                    followingOffset
                                )
                        else Nothing
            pure TransactionPage{transactions = page, nextCursor = cursor}

maximumQueryLength :: Int
maximumQueryLength = 2048

parseHledgerQuery :: Day -> Text -> Either AppError Hledger.Query
parseHledgerQuery referenceDate queryText =
    if Text.length queryText > maximumQueryLength
        then Left (QueryTooLong maximumQueryLength)
        else case queryText of
            "" -> Right Hledger.Any
            _ ->
                case Hledger.parseQuery referenceDate queryText of
                    Left parserError ->
                        Left
                            ( InvalidQuery
                                (safeParserReason parserError)
                                (parserPosition parserError)
                            )
                    Right (_, options@(_ : _)) ->
                        Left (UnsupportedQueryOption (safeOptionNames options))
                    Right (query, []) -> Right query

safeOptionNames :: [Hledger.QueryOpt] -> Text
safeOptionNames = Text.intercalate ", " . fmap optionName
  where
    optionName (Hledger.QueryOptInAcctOnly _) = "inacctonly:"
    optionName (Hledger.QueryOptInAcct _) = "inacct:"
    optionName (Hledger.QueryOptInterval _) = "interval:"

safeParserReason :: String -> Text
safeParserReason raw
    | "regular expression is invalid" `Text.isInfixOf` lower =
        "The query contains an invalid or unsupported regular expression."
    | "date parse error" `Text.isInfixOf` lower = "The query contains an invalid date expression."
    | "failed to parse query:" `Text.isPrefixOf` lower = "The boolean query expression is invalid."
    | "status" `Text.isInfixOf` lower = "The query contains an invalid status expression."
    | "amount" `Text.isInfixOf` lower = "The query contains an invalid amount expression."
    | otherwise = "The hledger query could not be parsed."
  where
    lower = Text.toLower (Text.pack raw)

parserPosition :: String -> Maybe Int
parserPosition raw = do
    suffix <- Text.stripPrefix "failed to parse query:" (Text.pack raw)
    let location = Text.takeWhile (/= '\n') suffix
        pieces = Text.splitOn ":" location
    column <-
        case pieces of
            _line : columnText : _ -> Just columnText
            _ -> Nothing
    if Text.all isDigit column && not (Text.null column)
        then readMaybe (Text.unpack column)
        else Nothing

emptyJournalReferenceDate :: Day
emptyJournalReferenceDate = fromGregorian 1970 1 1
