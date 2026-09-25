module Hedger.Domain.Internal.Account (
    AccountInfo (..),
    accountInfo,
    accountLabel,
    isCashOnly,
    isAssetOnly,
    isLiabilities,
    isRevenue,
    isEquity,
    isIncomeOrExpenses,
) where

import Data.Text (Text)
import Data.Text qualified as Text

import Hledger.Data.AccountName (accountNameInferType)
import Hledger.Data.Types (AccountName, AccountType (..))

data AccountInfo = AccountInfo
    { aname :: AccountName
    , atype :: Maybe AccountType
    }
    deriving stock (Eq, Ord, Show)

accountInfo :: AccountName -> AccountInfo
accountInfo name = AccountInfo{aname = name, atype = accountNameInferType name}

accountLabel :: AccountInfo -> Text
accountLabel ai =
    case ai.atype of
        Just Asset -> "assets"
        Just Liability -> "liabilities"
        Just Equity -> "equity"
        Just Revenue -> "income"
        Just Expense -> "expenses"
        Just Cash -> "assets"
        Just Conversion -> "equity"
        Just Gain -> "income"
        Nothing -> Text.toLower (Text.takeWhile (/= ':') ai.aname)

isEquity :: AccountInfo -> Bool
isEquity ai = ai.atype == Just Equity || ai.atype == Just Conversion

isIncomeOrExpenses :: AccountInfo -> Bool
isIncomeOrExpenses ai =
    case ai.atype of
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
isRevenue ai =
    case ai.atype of
        Just Revenue -> True
        Just Gain -> True
        _ -> False
