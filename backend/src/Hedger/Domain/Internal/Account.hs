module Hedger.Domain.Internal.Account (
    AccountInfo (aname, atype),
    DisplayDepth (..),
    accountInfo,
    accountLabel,
    displayDepthValue,
    isCashOnly,
    isAssetOnly,
    isLiabilities,
    isRevenue,
    isEquity,
    isIncomeOrExpenses,
    parseDisplayDepth,
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

-- | A depth to collapse account names to. Bounded to the four supported levels.
data DisplayDepth
    = Depth1
    | Depth2
    | Depth3
    | Depth4
    deriving stock (Eq, Ord, Show, Enum, Bounded)

displayDepthValue :: DisplayDepth -> Int
displayDepthValue depth =
    case depth of
        Depth1 -> 1
        Depth2 -> 2
        Depth3 -> 3
        Depth4 -> 4

parseDisplayDepth :: Int -> Maybe DisplayDepth
parseDisplayDepth value =
    case value of
        1 -> Just Depth1
        2 -> Just Depth2
        3 -> Just Depth3
        4 -> Just Depth4
        _ -> Nothing

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
