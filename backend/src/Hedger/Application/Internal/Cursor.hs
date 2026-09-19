module Hedger.Application.Internal.Cursor (
    encodeTransactionCursor,
    decodeTransactionCursor,
    encodeRegisterCursor,
    decodeRegisterCursor,
) where

import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Time (Day)
import Data.Word (Word8)
import Numeric (showHex)
import Text.Read (readMaybe)

import Hedger.Domain.Error (AppError (..))

-- Transaction
encodeTransactionCursor :: Text -> Text -> Day -> Int -> Text
encodeTransactionCursor revision query referenceDate offset =
    payload <> "." <> fingerprint ("cursor-check-v1:" <> payload)
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

decodeTransactionCursor :: Text -> Text -> Day -> Text -> Either AppError Int
decodeTransactionCursor revision query referenceDate cursor =
    case Text.splitOn "." cursor of
        [version, offsetText, cursorReference, queryHash, revisionHash, orderingHash, checksum]
            | version /= "v1" -> invalid "The cursor version is not supported."
            | checksum /= fingerprint ("cursor-check-v1:" <> payload) ->
                invalid "The cursor is malformed."
            | cursorReference /= Text.pack (show referenceDate) ->
                invalid "The cursor relative-date reference does not match this snapshot."
            | queryHash /= fingerprint query ->
                invalid "The cursor was created for a different query."
            | revisionHash /= fingerprint revision ->
                invalid "The cursor was created for a different journal revision."
            | orderingHash /= fingerprint ordering ->
                invalid "The cursor ordering is not supported."
            | otherwise ->
                maybe
                    (invalid "The cursor position is malformed.")
                    validateOffset
                    (readMaybe (Text.unpack offsetText))
          where
            payload =
                Text.intercalate
                    "."
                    [version, offsetText, cursorReference, queryHash, revisionHash, orderingHash]
        _ -> invalid "The cursor is malformed."
  where
    validateOffset offset
        | offset >= 0 = Right offset
        | otherwise = invalid "The cursor position is malformed."
    invalid = Left . InvalidCursor

ordering :: Text
ordering = "transaction-date-and-source-position-desc-v1"

-- Register
encodeRegisterCursor :: Text -> Text -> Text -> Maybe Day -> Maybe Day -> Int -> Text
encodeRegisterCursor revision account commodity start end offset =
    payload <> "." <> fingerprint ("register-cursor-check-v1:" <> payload)
  where
    payload =
        Text.intercalate
            "."
            [ "register-v1"
            , Text.pack (show offset)
            , fingerprint revision
            , fingerprint account
            , fingerprint commodity
            , fingerprint (canonicalDay start)
            , fingerprint (canonicalDay end)
            , fingerprint registerOrdering
            ]

decodeRegisterCursor ::
    Text -> Text -> Text -> Maybe Day -> Maybe Day -> Text -> Either AppError Int
decodeRegisterCursor revision account commodity start end cursor =
    case Text.splitOn "." cursor of
        [ version
            , offsetText
            , revisionHash
            , accountHash
            , commodityHash
            , startHash
            , endHash
            , orderingHash
            , checksum
            ]
                | version /= "register-v1" -> invalid "The cursor version is not supported."
                | checksum /= fingerprint ("register-cursor-check-v1:" <> payload) ->
                    invalid "The cursor is malformed."
                | revisionHash /= fingerprint revision ->
                    invalid "The cursor was created for a different journal revision."
                | accountHash /= fingerprint account ->
                    invalid "The cursor was created for a different account."
                | commodityHash /= fingerprint commodity ->
                    invalid "The cursor was created for a different commodity."
                | startHash /= fingerprint (canonicalDay start)
                    || endHash /= fingerprint (canonicalDay end) ->
                    invalid "The cursor was created for a different date range."
                | orderingHash /= fingerprint registerOrdering ->
                    invalid "The cursor ordering is not supported."
                | otherwise ->
                    maybe
                        (invalid "The cursor position is malformed.")
                        validateOffset
                        (readMaybe (Text.unpack offsetText))
              where
                payload =
                    Text.intercalate
                        "."
                        [ version
                        , offsetText
                        , revisionHash
                        , accountHash
                        , commodityHash
                        , startHash
                        , endHash
                        , orderingHash
                        ]
        _ -> invalid "The cursor is malformed."
  where
    validateOffset offset
        | offset >= 0 = Right offset
        | otherwise = invalid "The cursor position is malformed."
    invalid = Left . InvalidCursor

canonicalDay :: Maybe Day -> Text
canonicalDay = maybe "" (Text.pack . show)

registerOrdering :: Text
registerOrdering = "account-register-date-and-source-position-asc-v1"

-- Fingerprinting
fingerprint :: Text -> Text
fingerprint = hexEncode . MD5.hash . TextEncoding.encodeUtf8

hexEncode :: ByteString.ByteString -> Text
hexEncode = Text.pack . concatMap byteHex . ByteString.unpack

byteHex :: Word8 -> String
byteHex byte =
    case showHex byte "" of
        [digit] -> ['0', digit]
        digits -> digits
