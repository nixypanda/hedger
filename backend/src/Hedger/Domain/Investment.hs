module Hedger.Domain.Investment (
    AssetClass (..),
    AssetClassMapping (..),
    AssetClassMappings,
    InvestmentMappings,
    InvestmentMapping (..),
    PerformanceAccount (..),
    PerformanceRole (..),
    assetClassForAccount,
    assetClassMappingsList,
    isAssetClassAccount,
    assetClassScopeOverlaps,
    emptyAssetClassMappings,
    emptyInvestmentMappings,
    mkAssetClassMappings,
    mkInvestmentMappings,
    mappingsForScope,
    mappingForAccount,
    mappedAssetAccounts,
    accountWithin,
) where

import Data.Decimal (Decimal)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text

import Hledger.Data.AccountName qualified as HledgerAccountName

data AssetClass
    = Equity
    | Debt
    | Cash
    | Gold
    | Hybrid
    | Property
    | Other
    deriving stock (Eq, Ord, Show)

data AssetClassMapping = AssetClassMapping
    { assetClass :: AssetClass
    , accountPrefixes :: [Text]
    , targetPercentage :: Maybe Decimal
    }
    deriving stock (Eq, Show)

newtype AssetClassMappings
    = AssetClassMappings [AssetClassMapping]
    deriving stock (Eq, Show)

newtype InvestmentMappings
    = InvestmentMappings [InvestmentMapping]
    deriving stock (Eq, Show)

data InvestmentMapping = InvestmentMapping
    { assetAccount :: Text
    , performanceAccounts :: [PerformanceAccount]
    }
    deriving stock (Eq, Show)

data PerformanceAccount = PerformanceAccount
    { account :: Text
    , role :: PerformanceRole
    }
    deriving stock (Eq, Show)

data PerformanceRole
    = Interest
    | Dividend
    | RealisedGain
    | Fee
    | OtherIncome
    deriving stock (Eq, Show)

emptyInvestmentMappings :: InvestmentMappings
emptyInvestmentMappings = InvestmentMappings []

emptyAssetClassMappings :: AssetClassMappings
emptyAssetClassMappings = AssetClassMappings []

mkAssetClassMappings :: [AssetClassMapping] -> Either Text AssetClassMappings
mkAssetClassMappings mappings
    | any (null . (.accountPrefixes)) mappings =
        Left "Each asset class mapping requires at least one account prefix."
    | any (any invalidPrefix . (.accountPrefixes)) mappings =
        Left "Asset class mappings require fully qualified assets account prefixes."
    | any (maybe False (\target -> target < 0 || target > 100) . (.targetPercentage)) mappings =
        Left "Asset class target percentages must be between 0 and 100."
    | not (null targeted) && sum targeted /= 100 =
        Left "Configured asset class target percentages must total 100."
    | otherwise =
        case overlappingPrefixes of
            first : _ -> Left ("Asset class account-prefix scopes overlap: " <> first)
            [] -> Right (AssetClassMappings mappings)
  where
    invalidPrefix prefix = not (fullyQualified prefix) || not ("assets" `accountWithin` prefix)
    fullyQualified = (>= 2) . length . HledgerAccountName.accountNameComponents
    prefixes = concatMap (.accountPrefixes) mappings
    targeted = [target | mapping <- mappings, Just target <- [mapping.targetPercentage]]
    overlappingPrefixes =
        [ left
        | (leftIndex, left) <- zip [0 :: Int ..] prefixes
        , (rightIndex, right) <- zip [0 :: Int ..] prefixes
        , leftIndex < rightIndex
        , left `accountWithin` right || right `accountWithin` left
        ]

assetClassMappingsList :: AssetClassMappings -> [AssetClassMapping]
assetClassMappingsList (AssetClassMappings mappings) = mappings

assetClassForAccount :: Text -> AssetClassMappings -> AssetClass
assetClassForAccount account (AssetClassMappings mappings) =
    case [ mapping.assetClass
         | mapping <- mappings
         , prefix <- mapping.accountPrefixes
         , prefix `accountWithin` account
         ] of
        assetClass : _ -> assetClass
        [] -> Other

isAssetClassAccount :: Text -> AssetClassMappings -> Bool
isAssetClassAccount account (AssetClassMappings mappings) =
    any (\prefix -> prefix `accountWithin` account) (concatMap (.accountPrefixes) mappings)

assetClassScopeOverlaps :: Text -> AssetClassMappings -> Bool
assetClassScopeOverlaps account (AssetClassMappings mappings) =
    any
        (\prefix -> prefix `accountWithin` account || account `accountWithin` prefix)
        (concatMap (.accountPrefixes) mappings)

mkInvestmentMappings :: [InvestmentMapping] -> Either Text InvestmentMappings
mkInvestmentMappings mappings
    | any invalidMapping mappings =
        Left "Investment mappings require fully qualified asset and performance account names."
    | length assetAccounts /= length (List.nub assetAccounts) =
        Left "Each asset account may have only one investment mapping."
    | otherwise =
        case ambiguousPerformanceAccounts of
            first : _ ->
                Left
                    ( "Performance account scopes overlap and have ambiguous ownership: "
                        <> first
                    )
            [] -> Right (InvestmentMappings mappings)
  where
    invalidMapping mapping =
        not (fullyQualified mapping.assetAccount)
            || not ("assets" `accountWithin` mapping.assetAccount)
            || any (not . fullyQualified . (.account)) mapping.performanceAccounts
    fullyQualified = (>= 2) . length . HledgerAccountName.accountNameComponents
    owned =
        [ (performance.account, mapping.assetAccount)
        | mapping <- mappings
        , performance <- mapping.performanceAccounts
        ]
    assetAccounts = fmap (.assetAccount) mappings
    ambiguousPerformanceAccounts =
        [ leftAccount
        | (leftIndex, (leftAccount, _)) <- zip [0 :: Int ..] owned
        , (rightIndex, (rightAccount, _)) <- zip [0 :: Int ..] owned
        , leftIndex < rightIndex
        , leftAccount `accountWithin` rightAccount || rightAccount `accountWithin` leftAccount
        ]

mappingsForScope :: Text -> InvestmentMappings -> [InvestmentMapping]
mappingsForScope scope (InvestmentMappings mappings) =
    if Text.null scope
        then mappings
        else
            filter
                ( \mapping ->
                    scope `accountWithin` mapping.assetAccount
                        || mapping.assetAccount `accountWithin` scope
                )
                mappings

mappingForAccount :: Text -> InvestmentMappings -> Maybe InvestmentMapping
mappingForAccount asset (InvestmentMappings mappings) =
    maximumByMay
        compareDepth
        (filter (\mapping -> mapping.assetAccount `accountWithin` asset) mappings)
  where
    compareDepth left right = compare (depth left.assetAccount) (depth right.assetAccount)
    depth = length . HledgerAccountName.accountNameComponents

mappedAssetAccounts :: InvestmentMappings -> [Text]
mappedAssetAccounts (InvestmentMappings mappings) = fmap (.assetAccount) mappings

accountWithin :: Text -> Text -> Bool
accountWithin prefix account = prefix == account || (prefix <> ":") `Text.isPrefixOf` account

maximumByMay :: (value -> value -> Ordering) -> [value] -> Maybe value
maximumByMay _ [] = Nothing
maximumByMay compareValues (first : rest) = Just (List.foldl' choose first rest)
  where
    choose left right =
        case compareValues left right of
            GT -> left
            _ -> right
