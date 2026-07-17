module Hedger.Domain.Internal.Account (
    Account (..),
    accountLabel,
    accountText,
    accountGroups,
    accountOrder,
    accountOrderLabel,
    sameAccountGroup,
    topLevel,
    accountCount,
    sortedAccounts,
    isAssets,
    isExpenses,
    isEquity,
    isIncomeOrExpenses,
)
where

import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

data Account
    = Assets [Text]
    | Income [Text]
    | Equity [Text]
    | Liabilities [Text]
    | Expenses [Text]
    | Other Text [Text]
    deriving (Eq, Ord, Show)

topLevel :: Text -> Account
topLevel value =
    case Text.splitOn ":" value of
        [] -> Other "" []
        top : rest ->
            case Text.toLower top of
                "assets" -> Assets rest
                "income" -> Income rest
                "equity" -> Equity rest
                "liabilities" -> Liabilities rest
                "expenses" -> Expenses rest
                _ -> Other top rest

accountLabel :: Account -> Text
accountLabel account =
    case account of
        Assets _ -> "assets"
        Income _ -> "income"
        Equity _ -> "equity"
        Liabilities _ -> "liabilities"
        Expenses _ -> "expenses"
        Other top _ -> top

accountText :: Account -> Text
accountText account =
    Text.intercalate ":" $
        case account of
            Assets path -> "assets" : path
            Income path -> "income" : path
            Equity path -> "equity" : path
            Liabilities path -> "liabilities" : path
            Expenses path -> "expenses" : path
            Other top path -> top : path

accountGroups :: [Account] -> Set.Set Text
accountGroups = Set.fromList . fmap accountLabel

accountOrder :: Account -> Int
accountOrder account =
    case account of
        Assets _ -> 0
        Income _ -> 1
        Equity _ -> 2
        Liabilities _ -> 3
        Expenses _ -> 4
        Other _ _ -> 5

accountOrderLabel :: Text -> Int
accountOrderLabel label =
    case label of
        "assets" -> 0
        "income" -> 1
        "equity" -> 2
        "liabilities" -> 3
        "expenses" -> 4
        _ -> 5

sameAccountGroup :: Account -> Account -> Bool
sameAccountGroup left right = accountLabel left == accountLabel right

accountCount :: [Account] -> Int
accountCount = Set.size . Set.fromList

sortedAccounts :: [Account] -> [Account]
sortedAccounts =
    List.nubBy sameAccountGroup
        . List.sortOn accountOrder
        . Set.toList
        . Set.fromList

isAssets :: Account -> Bool
isAssets (Assets _) = True
isAssets _ = False

isExpenses :: Account -> Bool
isExpenses (Expenses _) = True
isExpenses _ = False

isEquity :: Account -> Bool
isEquity (Equity _) = True
isEquity _ = False

isIncomeOrExpenses :: Account -> Bool
isIncomeOrExpenses (Income _) = True
isIncomeOrExpenses (Expenses _) = True
isIncomeOrExpenses _ = False
