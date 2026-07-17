module Hedger.Infrastructure.Hledger.TransactionQuery (
    TransactionPage (..),
    defaultPageLimit,
    maximumPageLimit,
    maximumQueryLength,
    transactionPage,
)
where

import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString qualified as ByteString
import Data.Char (isDigit)
import Data.List qualified as List
import Data.Ord (Down (Down))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Time (Day, fromGregorian)
import Data.Word (Word8)
import Hedger.Domain.Error (AppError (..))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hledger.Data.Types qualified as Hledger
import Hledger.Query qualified as Hledger
import Numeric (showHex)
import Text.Read (readMaybe)

data TransactionPage = TransactionPage
    { transactions :: [Hledger.Transaction]
    , nextCursor :: Maybe Text
    }
    deriving (Show)

defaultPageLimit :: Int
defaultPageLimit = 50

maximumPageLimit :: Int
maximumPageLimit = 200

maximumQueryLength :: Int
maximumQueryLength = 2048

transactionPage :: Snapshot -> Maybe Text -> Maybe Text -> Maybe Text -> Either AppError TransactionPage
transactionPage snapshot suppliedQuery suppliedLimit suppliedCursor = do
    limit <- parseLimit suppliedLimit
    let canonicalQuery = Text.strip (maybe "" id suppliedQuery)
        referenceDate = maybe emptyJournalReferenceDate id snapshot.latestJournalDate
    if Text.length canonicalQuery > maximumQueryLength
        then Left (QueryTooLong maximumQueryLength)
        else pure ()
    parsedQuery <- parseHledgerQuery referenceDate canonicalQuery
    offset <- maybe (Right 0) (decodeCursor snapshot.revision canonicalQuery referenceDate) suppliedCursor
    let ordered = List.sortOn (\transaction -> (Down transaction.tdate, Down transaction.tindex)) snapshot.transactions
        matching = filter (Hledger.matchesTransaction parsedQuery) ordered
    if offset > length matching
        then Left (InvalidCursor "The cursor position is no longer valid.")
        else do
            let remaining = drop offset matching
                page = take limit remaining
                followingOffset = offset + length page
                cursor =
                    if length remaining > limit
                        then Just (encodeCursor snapshot.revision canonicalQuery referenceDate followingOffset)
                        else Nothing
            pure TransactionPage{transactions = page, nextCursor = cursor}

parseLimit :: Maybe Text -> Either AppError Int
parseLimit Nothing = Right defaultPageLimit
parseLimit (Just raw) =
    case readMaybe (Text.unpack raw) of
        Just value | value >= 1 && value <= maximumPageLimit -> Right value
        _ -> Left (InvalidLimit defaultPageLimit maximumPageLimit)

parseHledgerQuery :: Day -> Text -> Either AppError Hledger.Query
parseHledgerQuery _ "" = Right Hledger.Any
parseHledgerQuery referenceDate queryText =
    case Hledger.parseQuery referenceDate queryText of
        Left parserError -> Left (InvalidQuery (safeParserReason parserError) (parserPosition parserError))
        Right (_, options@(_ : _)) -> Left (UnsupportedQueryOption (safeOptionNames options))
        Right (query, []) -> Right query

safeOptionNames :: [Hledger.QueryOpt] -> Text
safeOptionNames = Text.intercalate ", " . fmap optionName
  where
    optionName (Hledger.QueryOptInAcctOnly _) = "inacctonly:"
    optionName (Hledger.QueryOptInAcct _) = "inacct:"
    optionName (Hledger.QueryOptInterval _) = "interval:"

safeParserReason :: String -> Text
safeParserReason raw
    | "regular expression is invalid" `Text.isInfixOf` lower = "The query contains an invalid or unsupported regular expression."
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
    column <- case pieces of
        _line : columnText : _ -> Just columnText
        _ -> Nothing
    if Text.all isDigit column && not (Text.null column)
        then readMaybe (Text.unpack column)
        else Nothing

encodeCursor :: Text -> Text -> Day -> Int -> Text
encodeCursor revision query referenceDate offset = payload <> "." <> fingerprint ("cursor-check-v1:" <> payload)
  where
    payload =
        Text.intercalate
            "."
            [ "v1"
            , Text.pack (show offset)
            , Text.pack (show referenceDate)
            , fingerprint query
            , fingerprint revision
            , fingerprint ordering
            ]

decodeCursor :: Text -> Text -> Day -> Text -> Either AppError Int
decodeCursor revision query referenceDate cursor =
    case Text.splitOn "." cursor of
        [version, offsetText, cursorReference, queryHash, revisionHash, orderingHash, checksum]
            | version /= "v1" -> invalid "The cursor version is not supported."
            | checksum /= fingerprint ("cursor-check-v1:" <> payload) -> invalid "The cursor is malformed."
            | cursorReference /= Text.pack (show referenceDate) -> invalid "The cursor relative-date reference does not match this snapshot."
            | queryHash /= fingerprint query -> invalid "The cursor was created for a different query."
            | revisionHash /= fingerprint revision -> invalid "The cursor was created for a different journal revision."
            | orderingHash /= fingerprint ordering -> invalid "The cursor ordering is not supported."
            | otherwise -> maybe (invalid "The cursor position is malformed.") validateOffset (readMaybe (Text.unpack offsetText))
          where
            payload = Text.intercalate "." [version, offsetText, cursorReference, queryHash, revisionHash, orderingHash]
        _ -> invalid "The cursor is malformed."
  where
    validateOffset offset
        | offset >= 0 = Right offset
        | otherwise = invalid "The cursor position is malformed."
    invalid = Left . InvalidCursor

ordering :: Text
ordering = "transaction-date-and-source-position-desc-v1"

emptyJournalReferenceDate :: Day
emptyJournalReferenceDate = fromGregorian 1970 1 1

fingerprint :: Text -> Text
fingerprint = hexEncode . MD5.hash . TextEncoding.encodeUtf8

hexEncode :: ByteString.ByteString -> Text
hexEncode = Text.pack . concatMap byteHex . ByteString.unpack

byteHex :: Word8 -> String
byteHex byte =
    case showHex byte "" of
        [digit] -> ['0', digit]
        digits -> digits
