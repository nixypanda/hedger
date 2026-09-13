module Hedger.Infrastructure.Internal.CursorSpec (cursorSpec) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day (ModifiedJulianDay))
import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hedger.Application.Error (AppError (..))
import Hedger.Application.Internal.Cursor (
    decodeRegisterCursor,
    decodeTransactionCursor,
    encodeRegisterCursor,
    encodeTransactionCursor,
 )

cursorSpec :: TestTree
cursorSpec =
    testGroup
        "cursor encode/decode"
        [ testGroup
            "transaction cursor"
            [ testProperty "roundtrip: decode . encode = Right offset" roundtrip
            , testProperty "rejects tampered checksum" rejectsTamperedChecksum
            , testProperty "rejects malformed cursors" rejectsMalformedCursors
            ]
        , testGroup
            "register cursor"
            [ testProperty "roundtrip: decode . encode = Right offset" registerRoundtrip
            , testProperty "rejects tampered checksum" registerRejectsTamperedChecksum
            , testProperty "rejects malformed cursors" registerRejectsMalformedCursors
            ]
        ]

-- * Transaction cursor

roundtrip :: Property
roundtrip = property $ do
    offset <- forAll genCursorPosition
    revision <- forAll genText
    query <- forAll genText
    referenceDate <- forAll genReferenceDate
    let cursor = encodeTransactionCursor revision query referenceDate offset
    decodeTransactionCursor revision query referenceDate cursor === Right offset

rejectsTamperedChecksum :: Property
rejectsTamperedChecksum = property $ do
    offset <- forAll genCursorPosition
    revision <- forAll genText
    query <- forAll genText
    referenceDate <- forAll genReferenceDate
    let cursor = encodeTransactionCursor revision query referenceDate offset
        -- Append extra text so the checksum is always wrong
        tampered = cursor <> "x"
    case decodeTransactionCursor revision query referenceDate tampered of
        Left (InvalidCursor _) -> return ()
        other -> fail $ "expected InvalidCursor, got: " <> show other

rejectsMalformedCursors :: Property
rejectsMalformedCursors = property $ do
    revision <- forAll genText
    query <- forAll genText
    referenceDate <- forAll genReferenceDate
    malformed <- forAll genMalformedCursor
    case decodeTransactionCursor revision query referenceDate malformed of
        Left (InvalidCursor _) -> return ()
        other -> fail $ "expected InvalidCursor, got: " <> show other

-- * Register cursor

registerRoundtrip :: Property
registerRoundtrip = property $ do
    offset <- forAll genCursorPosition
    revision <- forAll genText
    account <- forAll genText
    commodity <- forAll genText
    start <- forAll genOptionalDay
    end <- forAll genOptionalDay
    let cursor = encodeRegisterCursor revision account commodity start end offset
    decodeRegisterCursor revision account commodity start end cursor === Right offset

registerRejectsTamperedChecksum :: Property
registerRejectsTamperedChecksum = property $ do
    offset <- forAll genCursorPosition
    revision <- forAll genText
    account <- forAll genText
    commodity <- forAll genText
    start <- forAll genOptionalDay
    end <- forAll genOptionalDay
    let cursor = encodeRegisterCursor revision account commodity start end offset
        tampered = cursor <> "x"
    case decodeRegisterCursor revision account commodity start end tampered of
        Left (InvalidCursor _) -> return ()
        other -> fail $ "expected InvalidCursor, got: " <> show other

registerRejectsMalformedCursors :: Property
registerRejectsMalformedCursors = property $ do
    revision <- forAll genText
    account <- forAll genText
    commodity <- forAll genText
    start <- forAll genOptionalDay
    end <- forAll genOptionalDay
    malformed <- forAll genMalformedRegisterCursor
    case decodeRegisterCursor revision account commodity start end malformed of
        Left (InvalidCursor _) -> return ()
        other -> fail $ "expected InvalidCursor, got: " <> show other

-- * Generators

genCursorPosition :: Gen Int
genCursorPosition = Gen.int (Range.linear 0 10000)

genText :: Gen Text
genText = Text.pack <$> Gen.list (Range.linear 1 50) Gen.alphaNum

genReferenceDate :: Gen Day
genReferenceDate = ModifiedJulianDay <$> Gen.integral (Range.linear 40000 80000)

genOptionalDay :: Gen (Maybe Day)
genOptionalDay =
    Gen.choice
        [ pure Nothing
        , Just . ModifiedJulianDay <$> Gen.integral (Range.linear 40000 80000)
        ]

genMalformedCursor :: Gen Text
genMalformedCursor =
    Gen.choice
        [ pure ""
        , pure "not-a-cursor"
        , pure "v2.1.2024-01-01.abc.def.ghi.jkl"
        , pure "v1.abc.def.ghi.jkl.mno.pqr"
        , pure "v1.1.2024-01-01"
        , pure "v1.1.2024-01-01.abc.def.ghi.jkl.extra"
        ]

genMalformedRegisterCursor :: Gen Text
genMalformedRegisterCursor =
    Gen.choice
        [ pure ""
        , pure "not-a-cursor"
        , pure "register-v2.1.abc.def.ghi.jkl.mno.pqr.stu"
        , pure "register-v1.abc.def.ghi.jkl.mno.pqr.stu"
        , pure "register-v1.1.abc.def.ghi.jkl"
        , pure "register-v1.1.abc.def.ghi.jkl.mno.pqr.stu.extra"
        ]
