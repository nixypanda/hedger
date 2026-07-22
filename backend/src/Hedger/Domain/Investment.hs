module Hedger.Domain.Investment (
    InvestmentMappings,
    InvestmentMapping (..),
    PerformanceAccount (..),
    PerformanceRole (..),
    emptyInvestmentMappings,
    mkInvestmentMappings,
    mappingsForScope,
    mappingForAccount,
    mappedAssetAccounts,
)
where

import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text

import Hledger.Data.AccountName qualified as HledgerAccountName

newtype InvestmentMappings = InvestmentMappings [InvestmentMapping]
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

mkInvestmentMappings :: [InvestmentMapping] -> Either Text InvestmentMappings
mkInvestmentMappings mappings
    | any invalidMapping mappings =
        Left "Investment mappings require fully qualified asset and performance account names."
    | length assetAccounts /= length (List.nub assetAccounts) =
        Left "Each asset account may have only one investment mapping."
    | otherwise = case ambiguousPerformanceAccounts of
        first : _ -> Left ("Performance account scopes overlap and have ambiguous ownership: " <> first)
        [] -> Right (InvestmentMappings mappings)
  where
    invalidMapping mapping =
        not (fullyQualified mapping.assetAccount)
            || not ("assets" `isPrefixOf` mapping.assetAccount)
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
        , leftAccount `isPrefixOf` rightAccount || rightAccount `isPrefixOf` leftAccount
        ]

mappingsForScope :: Text -> InvestmentMappings -> [InvestmentMapping]
mappingsForScope scope (InvestmentMappings mappings) =
    if Text.null scope
        then mappings
        else filter (\mapping -> scope `isPrefixOf` mapping.assetAccount) mappings

mappingForAccount :: Text -> InvestmentMappings -> Maybe InvestmentMapping
mappingForAccount asset (InvestmentMappings mappings) =
    maximumByMay compareDepth (filter (\mapping -> mapping.assetAccount `isPrefixOf` asset) mappings)
  where
    compareDepth left right = compare (depth left.assetAccount) (depth right.assetAccount)
    depth = length . HledgerAccountName.accountNameComponents

mappedAssetAccounts :: InvestmentMappings -> [Text]
mappedAssetAccounts (InvestmentMappings mappings) = fmap (.assetAccount) mappings

isPrefixOf :: Text -> Text -> Bool
isPrefixOf prefix account = prefix == account || (prefix <> ":") `Text.isPrefixOf` account

maximumByMay :: (value -> value -> Ordering) -> [value] -> Maybe value
maximumByMay _ [] = Nothing
maximumByMay compareValues (first : rest) = Just (List.foldl' choose first rest)
  where
    choose left right = case compareValues left right of
        GT -> left
        _ -> right
