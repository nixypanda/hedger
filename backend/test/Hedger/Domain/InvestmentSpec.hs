module Hedger.Domain.InvestmentSpec (
    investmentSpec,
) where

import Data.Either (isLeft, isRight)
import Data.List qualified as List
import Data.Ord (comparing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Hedgehog (Gen, MonadTest, Property, assert, failure, footnote, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

import Hedger.Domain.Investment (
    AssetClass (..),
    AssetClassMapping (..),
    AssetClassMappings,
    InvestmentMapping (..),
    InvestmentMappings,
    PerformanceAccount (..),
    assetClassForAccount,
    mappingForAccount,
    mappingsForScope,
    mkAssetClassMappings,
    mkInvestmentMappings,
 )
import Hedger.Generators (
    genAccountComponent,
    genDisjointMappingSet,
    genPerformanceRole,
    genQualifiedAccountName,
 )

investmentSpec :: TestTree
investmentSpec =
    testGroup
        "investments"
        [ testGroup
            "asset class mappings"
            [ testCase "uses Other for unmatched accounts" unmatchedAccountsAreOther
            , testProperty
                "lookup does not depend on mapping order"
                assetClassLookupPermutationInvariant
            ]
        , testGroup
            "mkInvestmentMappings"
            [ testProperty "a disjoint, well-formed set is accepted" happyPath
            , testProperty "a duplicated asset account is rejected" rejectsDuplicateAssetAccount
            , testProperty "a single-component asset account is rejected" rejectsUnqualifiedAsset
            , testProperty "an asset account outside assets is rejected" rejectsAssetOutsideAssets
            , testProperty
                "overlapping performance scopes are rejected"
                rejectsOverlappingPerformanceScopes
            , testProperty "validity does not depend on mapping order" permutationInvariant
            ]
        , testGroup
            "mappingForAccount"
            [ testProperty
                "finds the deepest asset that is a component-prefix of the query"
                findsDeepestPrefix
            , testProperty
                "prefers the deepest mapping among a nested chain"
                findsDeepestAmongNested
            ]
        , testGroup
            "mappingsForScope"
            [ testProperty "an empty scope returns every mapping" emptyScopeReturnsAll
            , testProperty
                "a non-empty scope returns exactly the overlapping mappings"
                scopeReturnsOverlapping
            ]
        ]

-- Helpers
buildValid :: (MonadTest m) => [InvestmentMapping] -> m InvestmentMappings
buildValid mappings =
    case mkInvestmentMappings mappings of
        Right built -> pure built
        Left err -> footnote (Text.unpack err) >> failure

buildAssetClassMappings :: (MonadTest m) => [AssetClassMapping] -> m AssetClassMappings
buildAssetClassMappings mappings =
    case mkAssetClassMappings mappings of
        Right built -> pure built
        Left err -> footnote (Text.unpack err) >> failure

componentsOf :: Text -> [Text]
componentsOf = Text.splitOn ":"

isComponentPrefixOf :: Text -> Text -> Bool
isComponentPrefixOf prefix account = componentsOf prefix `List.isPrefixOf` componentsOf account

componentCount :: Text -> Int
componentCount = length . componentsOf

-- Asset class mappings
unmatchedAccountsAreOther :: IO ()
unmatchedAccountsAreOther = do
    mappings <-
        case mkAssetClassMappings [AssetClassMapping Equity ["assets:equity"] Nothing] of
            Left err -> assertFailure (Text.unpack err) >> fail "invalid asset class mapping"
            Right value -> pure value
    assertEqual "unmatched asset class" Other (assetClassForAccount "assets:debt:fund" mappings)

genAssetClass :: Gen AssetClass
genAssetClass = Gen.element [Equity, Debt, Cash, Gold, Hybrid, Property, Other]

genDisjointAssetClassMappingSet :: Gen [AssetClassMapping]
genDisjointAssetClassMappingSet = do
    mappingCount <- Gen.integral (Range.linear 1 5)
    leaves <- Set.toList <$> Gen.set (Range.singleton mappingCount) genAccountComponent
    classes <- Gen.list (Range.singleton mappingCount) genAssetClass
    pure
        [ AssetClassMapping assetClass ["assets:" <> leaf] Nothing
        | (assetClass, leaf) <- zip classes leaves
        ]

assetClassLookupPermutationInvariant :: Property
assetClassLookupPermutationInvariant =
    property $ do
        mappings <- forAll genDisjointAssetClassMappingSet
        shuffled <- forAll (Gen.shuffle mappings)
        first <- buildAssetClassMappings mappings
        second <- buildAssetClassMappings shuffled
        mapping <- forAll (Gen.element mappings)
        suffix <- forAll (Gen.list (Range.linear 0 3) genAccountComponent)
        case mapping.accountPrefixes of
            prefix : _ -> do
                let query = Text.intercalate ":" (prefix : suffix)
                assetClassForAccount query first === assetClassForAccount query second
            [] -> footnote "generator produced an empty prefix set" >> failure

-- mkInvestmentMappings
happyPath :: Property
happyPath =
    property $ do
        mappings <- forAll genDisjointMappingSet
        assert (isRight (mkInvestmentMappings mappings))

rejectsDuplicateAssetAccount :: Property
rejectsDuplicateAssetAccount =
    property $ do
        mappings <- forAll genDisjointMappingSet
        let duplicated = head mappings : mappings
        assert (isLeft (mkInvestmentMappings duplicated))

rejectsUnqualifiedAsset :: Property
rejectsUnqualifiedAsset =
    property $ do
        mappings <- forAll genDisjointMappingSet
        let bad = InvestmentMapping{assetAccount = "assets", performanceAccounts = []}
        assert (isLeft (mkInvestmentMappings (bad : mappings)))

rejectsAssetOutsideAssets :: Property
rejectsAssetOutsideAssets =
    property $ do
        mappings <- forAll genDisjointMappingSet
        outsideAsset <- forAll (genQualifiedAccountName "income")
        let bad = InvestmentMapping{assetAccount = outsideAsset, performanceAccounts = []}
        assert (isLeft (mkInvestmentMappings (bad : mappings)))

genOverlappingMappingPair :: Gen (InvestmentMapping, InvestmentMapping)
genOverlappingMappingPair = do
    base <- genQualifiedAccountName "income"
    extraComponents <- Gen.list (Range.linear 1 3) genAccountComponent
    firstRole <- genPerformanceRole
    secondRole <- genPerformanceRole
    let nested = Text.intercalate ":" (base : extraComponents)
        first =
            InvestmentMapping
                { assetAccount = "assets:one"
                , performanceAccounts = [PerformanceAccount base firstRole]
                }
        second =
            InvestmentMapping
                { assetAccount = "assets:two"
                , performanceAccounts = [PerformanceAccount nested secondRole]
                }
    pure (first, second)

rejectsOverlappingPerformanceScopes :: Property
rejectsOverlappingPerformanceScopes =
    property $ do
        (first, second) <- forAll genOverlappingMappingPair
        assert (isLeft (mkInvestmentMappings [first, second]))

permutationInvariant :: Property
permutationInvariant =
    property $ do
        mappings <- forAll genDisjointMappingSet
        shuffled <- forAll (Gen.shuffle mappings)
        isRight (mkInvestmentMappings mappings) === isRight (mkInvestmentMappings shuffled)

-- mappingForAccount
genQueryAccount :: [InvestmentMapping] -> Gen Text
genQueryAccount mappings = Gen.choice (genUnrelatedAccount : fmap extend mappings)
  where
    extend mapping = do
        extra <- Gen.list (Range.linear 0 3) (Gen.element ["x", "y", "z", "extra"])
        pure (Text.intercalate ":" (mapping.assetAccount : extra))
    genUnrelatedAccount = do
        segment <- Gen.element ["assets", "income", "expenses", "liabilities", "equity", "other"]
        genQualifiedAccountName segment

findsDeepestPrefix :: Property
findsDeepestPrefix =
    property $ do
        mappings <- forAll genDisjointMappingSet
        built <- buildValid mappings
        query <- forAll (genQueryAccount mappings)
        let candidates =
                filter
                    (\mapping -> mapping.assetAccount `isComponentPrefixOf` query)
                    mappings
            result = mappingForAccount query built
        case candidates of
            [] -> result === Nothing
            _ -> do
                let deepestDepth = maximum (fmap (componentCount . (.assetAccount)) candidates)
                case result of
                    Nothing -> footnote "expected a match" >> failure
                    Just found -> do
                        assert (found.assetAccount `isComponentPrefixOf` query)
                        componentCount found.assetAccount === deepestDepth

-- | A chain of mappings whose asset accounts are strictly nested prefixes of one another.
genNestedAssetChain :: Gen [InvestmentMapping]
genNestedAssetChain = do
    components <- Gen.list (Range.linear 2 4) genAccountComponent
    let prefixes =
            [ Text.intercalate ":" ("assets" : take size components)
            | size <- [1 .. length components]
            ]
    pure [InvestmentMapping{assetAccount = asset, performanceAccounts = []} | asset <- prefixes]

findsDeepestAmongNested :: Property
findsDeepestAmongNested =
    property $ do
        chain <- forAll genNestedAssetChain
        built <- buildValid chain
        extra <- forAll (Gen.list (Range.linear 0 2) genAccountComponent)
        let deepest =
                ( (.assetAccount)
                    ( List.maximumBy
                        (comparing (componentCount . (.assetAccount)))
                        chain
                    )
                )
            query = Text.intercalate ":" (deepest : extra)
        case mappingForAccount query built of
            Nothing -> footnote "expected a match for a query under the deepest asset" >> failure
            Just found -> found.assetAccount === deepest

-- mappingsForScope
emptyScopeReturnsAll :: Property
emptyScopeReturnsAll =
    property $ do
        mappings <- forAll genDisjointMappingSet
        built <- buildValid mappings
        mappingsForScope "" built === mappings

scopeReturnsOverlapping :: Property
scopeReturnsOverlapping =
    property $ do
        mappings <- forAll genDisjointMappingSet
        built <- buildValid mappings
        scope <- forAll (genQueryAccount mappings)
        let result = mappingsForScope scope built
            overlaps mapping =
                let asset = mapping.assetAccount
                 in scope `isComponentPrefixOf` asset || asset `isComponentPrefixOf` scope
        assert (all overlaps result)
        assert (all (not . overlaps) (mappings List.\\ result))
