module Hedger.Domain.Investment (
    AssetClass (..),
    AccountName,
    mkAccountName,
    accountNameText,
    AssetAccount,
    mkAssetAccount,
    assetAccountText,
    Percentage,
    mkPercentage,
    percentageValue,
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
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
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

-- | A fully qualified account name: at least two colon-separated components.
newtype AccountName = AccountName Text
    deriving stock (Eq, Ord, Show)

mkAccountName :: Text -> Either Text AccountName
mkAccountName name
    | fullyQualified name = Right (AccountName name)
    | otherwise = Left ("Account names must be fully qualified: " <> name)

accountNameText :: AccountName -> Text
accountNameText (AccountName name) = name

{- | An account rooted at @assets@: an asset-class prefix or the asset side of
an investment mapping.
-}
newtype AssetAccount = AssetAccount AccountName
    deriving stock (Eq, Ord, Show)

mkAssetAccount :: Text -> Either Text AssetAccount
mkAssetAccount name
    | "assets" `accountWithin` name = AssetAccount <$> mkAccountName name
    | otherwise = Left ("Asset accounts must be under assets: " <> name)

assetAccountText :: AssetAccount -> Text
assetAccountText (AssetAccount name) = accountNameText name

-- | A percentage in the closed interval @[0, 100]@.
newtype Percentage = Percentage Decimal
    deriving stock (Eq, Ord, Show)

mkPercentage :: Decimal -> Either Text Percentage
mkPercentage value
    | value < 0 || value > 100 = Left "Asset class target percentages must be between 0 and 100."
    | otherwise = Right (Percentage value)

percentageValue :: Percentage -> Decimal
percentageValue (Percentage value) = value

data AssetClassMapping = AssetClassMapping
    { assetClass :: AssetClass
    , accountPrefixes :: NonEmpty AssetAccount
    , targetPercentage :: Maybe Percentage
    }
    deriving stock (Eq, Show)

newtype AssetClassMappings
    = AssetClassMappings [AssetClassMapping]
    deriving stock (Eq, Show)

newtype InvestmentMappings
    = InvestmentMappings [InvestmentMapping]
    deriving stock (Eq, Show)

data InvestmentMapping = InvestmentMapping
    { assetAccount :: AssetAccount
    , performanceAccounts :: [PerformanceAccount]
    }
    deriving stock (Eq, Show)

data PerformanceAccount = PerformanceAccount
    { account :: AccountName
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
    | not (null targeted) && sum targeted /= 100 =
        Left "Configured asset class target percentages must total 100."
    | otherwise =
        case overlappingPrefixes of
            first : _ -> Left ("Asset class account-prefix scopes overlap: " <> first)
            [] -> Right (AssetClassMappings mappings)
  where
    prefixes =
        [ assetAccountText prefix
        | mapping <- mappings
        , prefix <- NonEmpty.toList mapping.accountPrefixes
        ]
    targeted =
        [percentageValue target | mapping <- mappings, Just target <- [mapping.targetPercentage]]
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
         , prefix <- NonEmpty.toList mapping.accountPrefixes
         , assetAccountText prefix `accountWithin` account
         ] of
        assetClass : _ -> assetClass
        [] -> Other

isAssetClassAccount :: Text -> AssetClassMappings -> Bool
isAssetClassAccount account (AssetClassMappings mappings) =
    any
        (\prefix -> assetAccountText prefix `accountWithin` account)
        (concatMap (NonEmpty.toList . (.accountPrefixes)) mappings)

assetClassScopeOverlaps :: Text -> AssetClassMappings -> Bool
assetClassScopeOverlaps account (AssetClassMappings mappings) =
    any
        ( \prefix ->
            assetAccountText prefix `accountWithin` account
                || account `accountWithin` assetAccountText prefix
        )
        (concatMap (NonEmpty.toList . (.accountPrefixes)) mappings)

mkInvestmentMappings :: [InvestmentMapping] -> Either Text InvestmentMappings
mkInvestmentMappings mappings
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
    owned =
        [ (accountNameText performance.account, assetAccountText mapping.assetAccount)
        | mapping <- mappings
        , performance <- mapping.performanceAccounts
        ]
    assetAccounts = fmap (assetAccountText . (.assetAccount)) mappings
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
                    scope `accountWithin` assetAccountText mapping.assetAccount
                        || assetAccountText mapping.assetAccount `accountWithin` scope
                )
                mappings

mappingForAccount :: Text -> InvestmentMappings -> Maybe InvestmentMapping
mappingForAccount asset (InvestmentMappings mappings) =
    maximumByMay
        compareDepth
        (filter (\mapping -> assetAccountText mapping.assetAccount `accountWithin` asset) mappings)
  where
    compareDepth left right =
        compare
            (depth (assetAccountText left.assetAccount))
            (depth (assetAccountText right.assetAccount))
    depth = length . HledgerAccountName.accountNameComponents

mappedAssetAccounts :: InvestmentMappings -> [AssetAccount]
mappedAssetAccounts (InvestmentMappings mappings) = fmap (.assetAccount) mappings

accountWithin :: Text -> Text -> Bool
accountWithin prefix account = prefix == account || (prefix <> ":") `Text.isPrefixOf` account

fullyQualified :: Text -> Bool
fullyQualified = (>= 2) . length . HledgerAccountName.accountNameComponents

maximumByMay :: (value -> value -> Ordering) -> [value] -> Maybe value
maximumByMay _ [] = Nothing
maximumByMay compareValues (first : rest) = Just (List.foldl' choose first rest)
  where
    choose left right =
        case compareValues left right of
            GT -> left
            _ -> right
