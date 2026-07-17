module Hedger.Domain.Internal.Account (
    AccountInfo (..),
    accountInfo,
    accountLabel,
    accountGroups,
    sortedAccountInfos,
    isAssets,
    isCashOnly,
    isAssetOnly,
    isLiabilities,
    isExpenses,
    isRevenue,
    isEquity,
    isIncomeOrExpenses,
)
where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Hledger.Data.AccountName (accountNameInferType)
import Hledger.Data.Types (AccountName, AccountType (..))

data AccountInfo = AccountInfo
    { aname :: AccountName
    , atype :: Maybe AccountType
    }
    deriving (Eq, Ord, Show)

accountInfo :: AccountName -> AccountInfo
accountInfo name =
    AccountInfo
        { aname = name
        , atype = accountNameInferType name
        }

accountLabel :: AccountInfo -> Text
accountLabel ai = case ai.atype of
    Just Asset -> "assets"
    Just Liability -> "liabilities"
    Just Equity -> "equity"
    Just Revenue -> "income"
    Just Expense -> "expenses"
    Just Cash -> "assets"
    Just Conversion -> "equity"
    Just Gain -> "income"
    Nothing -> Text.toLower (Text.takeWhile (/= ':') ai.aname)

accountGroups :: [AccountInfo] -> Set.Set Text
accountGroups = Set.fromList . fmap accountLabel

sortedAccountInfos :: [AccountInfo] -> [AccountInfo]
sortedAccountInfos = Set.toList . Set.fromList

isAssets :: AccountInfo -> Bool
isAssets ai = ai.atype == Just Asset || ai.atype == Just Cash

isExpenses :: AccountInfo -> Bool
isExpenses ai = ai.atype == Just Expense

isEquity :: AccountInfo -> Bool
isEquity ai = ai.atype == Just Equity || ai.atype == Just Conversion

isIncomeOrExpenses :: AccountInfo -> Bool
isIncomeOrExpenses ai = case ai.atype of
    Just Revenue -> True
    Just Gain -> True
    Just Expense -> True
    _ -> False

isCashOnly :: AccountInfo -> Bool
isCashOnly ai = ai.atype == Just Cash

isAssetOnly :: AccountInfo -> Bool
isAssetOnly ai = ai.atype == Just Asset

isLiabilities :: AccountInfo -> Bool
isLiabilities ai = ai.atype == Just Liability

isRevenue :: AccountInfo -> Bool
isRevenue ai = case ai.atype of
    Just Revenue -> True
    Just Gain -> True
    _ -> False
