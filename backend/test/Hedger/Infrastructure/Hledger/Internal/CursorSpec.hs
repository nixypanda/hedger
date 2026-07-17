module Hedger.Infrastructure.Hledger.Internal.CursorSpec (cursorSpec) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day (ModifiedJulianDay))
import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Hedger.Domain.Error (AppError (..))
import Hedger.Infrastructure.Hledger.Internal.Cursor (decodeCursor, encodeCursor)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

cursorSpec :: TestTree
cursorSpec =
    testGroup
        "cursor encode/decode"
        [ testProperty "roundtrip: decode . encode = Right offset" roundtrip
        , testProperty "rejects tampered checksum" rejectsTamperedChecksum
        , testProperty "rejects malformed cursors" rejectsMalformedCursors
        ]

roundtrip :: Property
roundtrip = property $ do
    offset <- forAll genCursorPosition
    revision <- forAll genText
    query <- forAll genText
    referenceDate <- forAll genReferenceDate
    let cursor = encodeCursor revision query referenceDate offset
    decodeCursor revision query referenceDate cursor === Right offset

rejectsTamperedChecksum :: Property
rejectsTamperedChecksum = property $ do
    offset <- forAll genCursorPosition
    revision <- forAll genText
    query <- forAll genText
    referenceDate <- forAll genReferenceDate
    let cursor = encodeCursor revision query referenceDate offset
        -- Append extra text so the checksum is always wrong
        tampered = cursor <> "x"
    case decodeCursor revision query referenceDate tampered of
        Left (InvalidCursor _) -> return ()
        other -> fail $ "expected InvalidCursor, got: " <> show other

rejectsMalformedCursors :: Property
rejectsMalformedCursors = property $ do
    revision <- forAll genText
    query <- forAll genText
    referenceDate <- forAll genReferenceDate
    malformed <- forAll genMalformedCursor
    case decodeCursor revision query referenceDate malformed of
        Left (InvalidCursor _) -> return ()
        other -> fail $ "expected InvalidCursor, got: " <> show other

genCursorPosition :: Gen Int
genCursorPosition = Gen.int (Range.linear 0 10000)

genText :: Gen Text
genText = Text.pack <$> Gen.list (Range.linear 1 50) Gen.alphaNum

genReferenceDate :: Gen Day
genReferenceDate = ModifiedJulianDay <$> Gen.integral (Range.linear 40000 80000)

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
