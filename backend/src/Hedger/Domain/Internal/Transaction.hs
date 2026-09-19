module Hedger.Domain.Internal.Transaction (
    -- * Types
    TransactionClassification (..),
    TransactionPresentation (..),

    -- * Core API
    newestFirst,
    presentationFor,

    -- * Domain queries
    classificationFor,
    dominantMovement,
    isOpening,
) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Ord (Down (Down))
import Data.Text qualified as Text

import Hledger.Data.Amount qualified as Hledger
import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Internal.Account

data TransactionClassification
    = OpeningBalance
    | Borrowing
    | Repayment
    | AssetAcquisition
    | AssetDisposal
    | AssetReallocation
    | TransactionIncome
    | TransactionExpense
    | TransactionTransfer
    | Unclassified
    deriving stock (Eq, Show)

data TransactionPresentation = TransactionPresentation
    { classification :: TransactionClassification
    }
    deriving stock (Eq, Show)

newestFirst :: [Hledger.Transaction] -> [Hledger.Transaction]
newestFirst = List.sortOn (\transaction -> (Down transaction.tdate, Down transaction.tindex))

-- * Domain queries
classificationThreshold :: Double
classificationThreshold = 0.8

classificationFor :: Hledger.Transaction -> TransactionClassification
classificationFor tx
    | isOpening tx = OpeningBalance
    | dominantPct < classificationThreshold = Unclassified
    | any isLiabilities creditAccounts && any isLiabilities debitAccounts = TransactionTransfer
    | any isLiabilities creditAccounts = Borrowing
    | dominantGroup == "liabilities" && any isLiabilities debitAccounts = Repayment
    | dominantGroup == "assets" && any isCashOnly creditAccounts && any isAssetOnly debitAccounts =
        AssetAcquisition
    | dominantGroup == "assets" && any isAssetOnly creditAccounts && any isCashOnly debitAccounts =
        AssetDisposal
    | dominantGroup == "assets" && any isAssetOnly creditAccounts && any isAssetOnly debitAccounts =
        AssetReallocation
    | dominantGroup == "assets" && any isRevenue creditAccounts = TransactionIncome
    | dominantGroup == "expenses" = TransactionExpense
    | otherwise = TransactionTransfer
  where
    creditAccounts = postingAccountInfos (creditsFor tx.tpostings)
    debitAccounts = postingAccountInfos (debitsFor tx.tpostings)
    (dominantGroup, dominantPct) = dominantMovement tx

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
            ["opening", "open", "balance", "balances", "conversion", "migration", "initial"]

dominantMovement :: Hledger.Transaction -> (Text.Text, Double)
dominantMovement tx = (dominantGroup, dominantPct)
  where
    debitPs = debitsFor tx.tpostings
    groupAmounts =
        Map.fromListWith
            (+)
            [ ( accountLabel (accountInfo p.paccount)
              , abs (fromRational (toRational amount.aquantity))
              )
            | p <- debitPs
            , amount <- Hledger.amountsRaw p.pamount
            , amount.aquantity > 0
            ]
    totalDebit = sum (Map.elems groupAmounts)
    dominantGroup =
        if Map.null groupAmounts
            then ""
            else
                fst
                    ( Map.foldlWithKey'
                        ( \(!bestKey, !bestVal) candidateKey candidateVal ->
                            if candidateVal > bestVal
                                then (candidateKey, candidateVal)
                                else (bestKey, bestVal)
                        )
                        ("", 0.0)
                        groupAmounts
                    )
    dominantPct =
        if totalDebit == 0
            then 0.0
            else case Map.lookup dominantGroup groupAmounts of
                Just amount -> amount / totalDebit
                Nothing -> 0.0

-- * Core API
presentationFor :: Hledger.Transaction -> TransactionPresentation
presentationFor tx = TransactionPresentation{classification = classificationFor tx}

-- * Internal (shared between domain functions)
postingAccountInfos :: [Hledger.Posting] -> [AccountInfo]
postingAccountInfos = fmap (accountInfo . (.paccount))

creditsFor :: [Hledger.Posting] -> [Hledger.Posting]
creditsFor = filter (\p -> any ((< 0) . (.aquantity)) (Hledger.amountsRaw p.pamount))

debitsFor :: [Hledger.Posting] -> [Hledger.Posting]
debitsFor = filter (\p -> any ((> 0) . (.aquantity)) (Hledger.amountsRaw p.pamount))
