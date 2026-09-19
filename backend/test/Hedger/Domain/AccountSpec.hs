module Hedger.Domain.AccountSpec (
    accountSpec,
) where

import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as Text
import Hedgehog (Gen, Property, assert, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hedger.Domain.Account (atDepth)

accountSpec :: TestTree
accountSpec =
    testGroup
        "atDepth"
        [ testProperty "result components are a prefix of the account's components" atDepthIsPrefix
        , testProperty "component count matches the clamped depth" atDepthComponentCount
        , testProperty "idempotent" atDepthIdempotent
        , testProperty
            "deep depths leave the first (up to) 4 components unchanged"
            atDepthDeepUnchanged
        ]

genAccountComponent :: Gen Text
genAccountComponent = Text.pack <$> Gen.list (Range.linear 1 8) Gen.alphaNum

genAccountName :: Gen Text
genAccountName = Text.intercalate ":" <$> Gen.list (Range.linear 1 6) genAccountComponent

genDepth :: Gen Int
genDepth = Gen.integral (Range.linearFrom 0 (-5) 10)

clampDepth :: Int -> Int
clampDepth depth = max 1 (min 4 depth)

atDepthIsPrefix :: Property
atDepthIsPrefix =
    property $ do
        account <- forAll genAccountName
        depth <- forAll genDepth
        let resultComponents = Text.splitOn ":" (atDepth depth account)
            accountComponents = Text.splitOn ":" account
        assert (resultComponents `isPrefixOf` accountComponents)

atDepthComponentCount :: Property
atDepthComponentCount =
    property $ do
        account <- forAll genAccountName
        depth <- forAll genDepth
        let accountComponents = Text.splitOn ":" account
            resultComponents = Text.splitOn ":" (atDepth depth account)
        length resultComponents === min (clampDepth depth) (length accountComponents)

atDepthIdempotent :: Property
atDepthIdempotent =
    property $ do
        account <- forAll genAccountName
        depth <- forAll genDepth
        atDepth depth (atDepth depth account) === atDepth depth account

atDepthDeepUnchanged :: Property
atDepthDeepUnchanged =
    property $ do
        account <- forAll genAccountName
        let accountComponents = Text.splitOn ":" account
            threshold = min 4 (length accountComponents)
        extra <- forAll (Gen.integral (Range.linear 0 20))
        let depth = threshold + extra
        take threshold accountComponents === Text.splitOn ":" (atDepth depth account)
