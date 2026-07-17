module Hedger.Domain.Internal.Transaction (
    -- * Types
    CommodityQuantity (..),
    TransactionClassification (..),
    TransactionPresentation (..),
    Entry (..),
    FlowTone (..),

    -- * Core API
    newestFirst,
    presentationFor,

    -- * Internal helpers (exported for testing)
    postingEntries,
    sortedAccountsForEntries,
    toneFromFlow,
    dominantToAccounts,
    relevantEntries,
    aggregateAmounts,
    isOpeningSetup,
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

import Hedger.Domain.Internal.Account

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
    , fromAccounts :: [Text]
    , toAccounts :: [Text]
    , totalAmounts :: [CommodityQuantity]
    , openingAccountCount :: Maybe Int
    }
    deriving (Eq, Show)

newestFirst :: [Hledger.Transaction] -> [Hledger.Transaction]
newestFirst = List.sortOn (\transaction -> (Down transaction.tdate, Down transaction.tindex))

data Entry = Entry
    { account :: Account
    , amount :: Hledger.Amount
    }
    deriving (Eq)

data FlowTone = Positive | Negative | Neutral
    deriving (Eq, Show)

presentationFor :: Hledger.Transaction -> TransactionPresentation
presentationFor transaction =
    TransactionPresentation
        { classification = classification
        , fromAccounts = fromAccounts
        , toAccounts = toAccounts
        , totalAmounts = aggregateAmounts selectedEntries
        , openingAccountCount = if opening then Just (accountCount (fmap (.account) debits)) else Nothing
        }
  where
    entries = postingEntries transaction.tpostings
    debits = filter ((> 0) . (.amount.aquantity)) entries
    credits = filter ((< 0) . (.amount.aquantity)) entries
    rawFromAccounts = sortedAccountsForEntries credits
    rawToAccounts = sortedAccountsForEntries debits
    opening = isOpeningSetup credits debits
    fromAccounts = if opening then ["opening"] else accountLabel <$> rawFromAccounts
    toAccounts =
        if opening
            then accountLabel <$> rawToAccounts
            else accountLabel <$> dominantToAccounts rawFromAccounts rawToAccounts debits
    tone = toneFromFlow rawFromAccounts rawToAccounts
    classification
        | opening = OpeningBalance
        | tone == Positive = TransactionIncome
        | tone == Negative = TransactionExpense
        | otherwise = TransactionTransfer
    selectedEntries
        | opening = []
        | otherwise = relevantEntries tone rawFromAccounts rawToAccounts credits debits

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

isOpeningSetup :: [Entry] -> [Entry] -> Bool
isOpeningSetup credits debits =
    any isEquity accounts
        && not (any isIncomeOrExpenses accounts)
        && not (null debits)
        && any hasOpeningSource credits
  where
    accounts = (.account) <$> credits <> debits

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
