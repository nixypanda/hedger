module Hedger.Domain.AccountSpec (
    accountSpec,
) where

import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as Text
import Hedgehog (Gen, Property, assert, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Hedger.Domain.Account (DisplayDepth (..), atDepth)

accountSpec :: TestTree
accountSpec =
    testGroup
        "atDepth"
        [testProperty "result components are a prefix of the account's components" atDepthIsPrefix]

genAccountComponent :: Gen Text
genAccountComponent = Text.pack <$> Gen.list (Range.linear 1 8) Gen.alphaNum

genAccountName :: Gen Text
genAccountName = Text.intercalate ":" <$> Gen.list (Range.linear 1 6) genAccountComponent

genDepth :: Gen DisplayDepth
genDepth = Gen.element [minBound .. maxBound]

atDepthIsPrefix :: Property
atDepthIsPrefix =
    property $ do
        account <- forAll genAccountName
        depth <- forAll genDepth
        let resultComponents = Text.splitOn ":" (atDepth depth account)
            accountComponents = Text.splitOn ":" account
        assert (resultComponents `isPrefixOf` accountComponents)
