module Hedger.Domain.Internal.Transaction (
    -- * Types
    TransactionClassification (..),
    TransactionPresentation (..),

    -- * Core API
    newestFirst,
    presentationFor,

    -- * Domain queries
    classificationFor,
    isOpening,
)
where

import Data.List qualified as List
import Data.Ord (Down (Down))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Hledger.Data.Amount qualified as Hledger
import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Internal.Account

data TransactionClassification
    = TransactionIncome
    | TransactionExpense
    | TransactionTransfer
    | OpeningBalance
    deriving (Eq, Show)

data TransactionPresentation = TransactionPresentation
    { classification :: TransactionClassification
    }
    deriving (Eq, Show)

newestFirst :: [Hledger.Transaction] -> [Hledger.Transaction]
newestFirst = List.sortOn (\transaction -> (Down transaction.tdate, Down transaction.tindex))

-- * Domain queries

classificationFor :: Hledger.Transaction -> TransactionClassification
classificationFor tx
    | isOpening tx = OpeningBalance
    | tone == Positive = TransactionIncome
    | tone == Negative = TransactionExpense
    | otherwise = TransactionTransfer
  where
    tone =
        toneFromFlow
            (sortedAccountsForPostings (creditsFor tx.tpostings))
            (sortedAccountsForPostings (debitsFor tx.tpostings))

isOpening :: Hledger.Transaction -> Bool
isOpening tx =
    any isEquity accounts
        && not (any isIncomeOrExpenses accounts)
        && not (null debitPs)
        && any hasOpeningSource creditPs
  where
    creditPs = creditsFor tx.tpostings
    debitPs = debitsFor tx.tpostings
    accounts = postingAccountInfos creditPs <> postingAccountInfos debitPs

    hasOpeningSource posting =
        any
            (`Text.isInfixOf` Text.toLower posting.paccount)
            [ "opening"
            , "open"
            , "balance"
            , "balances"
            , "conversion"
            , "migration"
            , "initial"
            ]

-- * Core API

presentationFor :: Hledger.Transaction -> TransactionPresentation
presentationFor tx =
    TransactionPresentation
        { classification = classificationFor tx
        }

-- * Internal (shared between domain functions)

postingAccountInfos :: [Hledger.Posting] -> [AccountInfo]
postingAccountInfos = fmap (accountInfo . (.paccount))

sortedAccountsForPostings :: [Hledger.Posting] -> [AccountInfo]
sortedAccountsForPostings = sortedAccountInfos . postingAccountInfos

creditsFor :: [Hledger.Posting] -> [Hledger.Posting]
creditsFor = filter (\p -> any ((< 0) . (.aquantity)) (Hledger.amountsRaw p.pamount))

debitsFor :: [Hledger.Posting] -> [Hledger.Posting]
debitsFor = filter (\p -> any ((> 0) . (.aquantity)) (Hledger.amountsRaw p.pamount))

data FlowTone = Positive | Negative | Neutral
    deriving (Eq, Show)

toneFromFlow :: [AccountInfo] -> [AccountInfo] -> FlowTone
toneFromFlow fromAccounts toAccounts
    | not (Set.null fromSet) && fromSet == toSet = Neutral
    | any isExpenses toAccounts = Negative
    | any isAssets toAccounts && any (not . isAssets) fromAccounts = Positive
    | otherwise = Neutral
  where
    fromSet = accountGroups fromAccounts
    toSet = accountGroups toAccounts
