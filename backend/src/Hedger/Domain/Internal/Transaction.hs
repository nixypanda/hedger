{-# OPTIONS_GHC -Wno-orphans #-}

module Hedger.Domain.Internal.Transaction (
    -- * Types
    CommodityQuantity (..),
    TransactionClassification (..),
    TransactionPresentation (..),

    -- * Core API
    newestFirst,
    presentationFor,

    -- * Domain queries
    classificationFor,
    isOpening,
    fromAccountsFor,
    toAccountsFor,
    dominantToAccountFor,

    -- * Internal (shared between domain functions)
    postingEntries,
    sortedAccountsForEntries,
    dominantToAccounts,
)
where

import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (Down))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Hledger.Data.Amount qualified as Hledger
import Hledger.Data.Types qualified as Hledger

import GHC.Records (HasField (getField))
import Hedger.Domain.Internal.Account

instance HasField "entries" Hledger.Transaction [Entry] where
    getField :: Hledger.Transaction -> [Entry]
    getField tx = postingEntries tx.tpostings

instance HasField "credits" Hledger.Transaction [Entry] where
    getField :: Hledger.Transaction -> [Entry]
    getField tx = filter ((< 0) . (.amount.aquantity)) tx.entries

instance HasField "debits" Hledger.Transaction [Entry] where
    getField :: Hledger.Transaction -> [Entry]
    getField tx = filter ((> 0) . (.amount.aquantity)) tx.entries

data CommodityQuantity = CommodityQuantity
    { commodity :: Text
    , quantity :: Hledger.Quantity
    }
    deriving (Eq, Show)

data TransactionClassification
    = TransactionIncome
    | TransactionExpense
    | TransactionTransfer
    | OpeningBalance
    deriving (Eq, Show)

data TransactionPresentation = TransactionPresentation
    { classification :: TransactionClassification
    , fromAccounts :: [Account]
    , toAccounts :: [Account]
    , totalAmounts :: [CommodityQuantity]
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
    tone = toneFromFlow (sortedAccountsForEntries tx.credits) (sortedAccountsForEntries tx.debits)

isOpening :: Hledger.Transaction -> Bool
isOpening tx =
    any isEquity accounts
        && not (any isIncomeOrExpenses accounts)
        && not (null tx.debits)
        && any hasOpeningSource tx.credits
  where
    accounts = (.account) <$> tx.credits <> tx.debits

    hasOpeningSource entry =
        any
            (`Text.isInfixOf` Text.toLower (accountText entry.account))
            [ "opening"
            , "open"
            , "balance"
            , "balances"
            , "conversion"
            , "migration"
            , "initial"
            ]

fromAccountsFor :: Hledger.Transaction -> [Account]
fromAccountsFor tx = sortedAccountsForEntries tx.credits

toAccountsFor :: Hledger.Transaction -> [Account]
toAccountsFor tx = sortedAccountsForEntries tx.debits

dominantToAccountFor :: Hledger.Transaction -> Maybe Account
dominantToAccountFor tx
    | length rawFromAccounts == 1 && length rawToAccounts > 1 =
        case sortedTotals of
            [] -> Nothing
            (account, _) : _ -> Just account
    | otherwise = Nothing
  where
    rawFromAccounts = sortedAccountsForEntries tx.credits
    rawToAccounts = sortedAccountsForEntries tx.debits
    totals =
        Map.fromListWith
            (+)
            [(entry.account, abs entry.amount.aquantity) | entry <- tx.debits]
    sortedTotals = List.sortBy compareTotals (Map.toList totals)
    compareTotals (leftAccount, leftTotal) (rightAccount, rightTotal)
        | leftTotal == rightTotal =
            compare (accountOrderLabel (accountLabel leftAccount)) (accountOrderLabel (accountLabel rightAccount))
        | otherwise = compare rightTotal leftTotal

-- * Core API

presentationFor :: Hledger.Transaction -> TransactionPresentation
presentationFor tx =
    TransactionPresentation
        { classification = classificationFor tx
        , fromAccounts = fromAccountsFor tx
        , toAccounts = toAccountsFor tx
        , totalAmounts = aggregateAmounts selectedEntries
        }
  where
    opening = isOpening tx
    rawFromAccounts = sortedAccountsForEntries tx.credits
    rawToAccounts = sortedAccountsForEntries tx.debits
    tone = toneFromFlow rawFromAccounts rawToAccounts
    selectedEntries
        | opening = []
        | otherwise = relevantEntries tone rawFromAccounts rawToAccounts tx.credits tx.debits

-- * Internal (shared between domain functions)

data Entry = Entry
    { account :: Account
    , amount :: Hledger.Amount
    }
    deriving (Eq)

postingEntries :: [Hledger.Posting] -> [Entry]
postingEntries =
    concatMap
        ( \posting ->
            [ Entry
                { account = topLevel posting.paccount
                , amount = amount
                }
            | amount <- Hledger.amountsRaw posting.pamount
            , amount.aquantity /= 0
            ]
        )

sortedAccountsForEntries :: [Entry] -> [Account]
sortedAccountsForEntries =
    sortedAccounts . fmap (.account)

data FlowTone = Positive | Negative | Neutral
    deriving (Eq, Show)

toneFromFlow :: [Account] -> [Account] -> FlowTone
toneFromFlow fromAccounts toAccounts
    | not (Set.null fromSet) && fromSet == toSet = Neutral
    | any isExpenses toAccounts = Negative
    | any isAssets toAccounts && any (not . isAssets) fromAccounts = Positive
    | otherwise = Neutral
  where
    fromSet = accountGroups fromAccounts
    toSet = accountGroups toAccounts

dominantToAccounts :: [Account] -> [Account] -> [Entry] -> [Account]
dominantToAccounts fromAccounts toAccounts debitEntries
    | length fromAccounts == 1 && length toAccounts > 1 =
        case sortedTotals of
            [] -> toAccounts
            (label, _) : _ -> maybe toAccounts pure (List.find ((== label) . accountLabel) toAccounts)
    | otherwise = toAccounts
  where
    totals =
        Map.fromListWith
            (+)
            [(accountLabel entry.account, abs entry.amount.aquantity) | entry <- debitEntries]
    sortedTotals = List.sortBy compareTotals (Map.toList totals)
    compareTotals (leftAccount, leftTotal) (rightAccount, rightTotal)
        | leftTotal == rightTotal =
            compare (accountOrderLabel leftAccount) (accountOrderLabel rightAccount)
        | otherwise = compare rightTotal leftTotal

relevantEntries :: FlowTone -> [Account] -> [Account] -> [Entry] -> [Entry] -> [Entry]
relevantEntries tone fromAccounts toAccounts credits debits
    | null selected = debits <> credits
    | otherwise = selected
  where
    selected
        | tone == Neutral
            && not (null fromAccounts)
            && accountGroups fromAccounts == accountGroups toAccounts =
            filter (accountIn fromAccounts . (.account)) credits
        | otherwise = filter (accountIn toAccounts . (.account)) debits

    accountIn accounts account = accountLabel account `Set.member` accountGroups accounts

aggregateAmounts :: [Entry] -> [CommodityQuantity]
aggregateAmounts entries =
    [ CommodityQuantity
        { commodity = commodity
        , quantity = quantity
        }
    | (commodity, quantity) <- Map.toAscList totals
    ]
  where
    totals :: Map Text Hledger.Quantity
    totals =
        Map.fromListWith
            (+)
            [(entry.amount.acommodity, abs entry.amount.aquantity) | entry <- entries]
