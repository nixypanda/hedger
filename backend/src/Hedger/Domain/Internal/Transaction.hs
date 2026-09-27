module Hedger.Domain.Internal.Transaction (
    -- * Types
    TransactionClassification (..),
    TransactionPresentation (..),
    AccountGroup (..),
    DominantMovement (..),
    Fraction,

    -- * Core API
    newestFirst,
    presentationFor,

    -- * Domain queries
    classificationFor,
    dominantMovement,
    isOpening,
    accountGroup,
    fraction,
    fractionValue,
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

{- | A top-level account group, as produced by 'accountLabel'. Unknown groups
keep their raw label so behaviour matches the previous string grouping.
-}
data AccountGroup
    = Assets
    | Liabilities
    | Equity
    | Income
    | Expenses
    | OtherGroup Text.Text
    deriving stock (Show)

-- | Ordering mirrors the label text, so grouping and tie-breaking are unchanged.
instance Eq AccountGroup where
    left == right = accountGroupLabel left == accountGroupLabel right

instance Ord AccountGroup where
    compare left right = compare (accountGroupLabel left) (accountGroupLabel right)

accountGroupLabel :: AccountGroup -> Text.Text
accountGroupLabel group =
    case group of
        Assets -> "assets"
        Liabilities -> "liabilities"
        Equity -> "equity"
        Income -> "income"
        Expenses -> "expenses"
        OtherGroup label -> label

accountGroup :: AccountInfo -> AccountGroup
accountGroup = groupFromLabel . accountLabel
  where
    groupFromLabel label =
        case label of
            "assets" -> Assets
            "liabilities" -> Liabilities
            "equity" -> Equity
            "income" -> Income
            "expenses" -> Expenses
            other -> OtherGroup other

-- | A fraction in the closed interval [0, 1]. The constructor is not exported.
newtype Fraction = Fraction Rational
    deriving stock (Eq, Ord, Show)

-- | Reject values outside [0, 1]. Rational values are always finite.
fraction :: Rational -> Maybe Fraction
fraction value
    | value >= 0 && value <= 1 = Just (Fraction value)
    | otherwise = Nothing

fractionValue :: Fraction -> Rational
fractionValue (Fraction value) = value

data DominantMovement
    = Dominant AccountGroup Fraction
    | NoDebits
    deriving stock (Eq, Show)

newestFirst :: [Hledger.Transaction] -> [Hledger.Transaction]
newestFirst = List.sortOn (\transaction -> (Down transaction.tdate, Down transaction.tindex))

-- * Domain queries
classificationThreshold :: Fraction
classificationThreshold = Fraction 0.8

classificationFor :: Hledger.Transaction -> TransactionClassification
classificationFor tx
    | isOpening tx = OpeningBalance
    | otherwise =
        case dominantMovement tx of
            NoDebits -> Unclassified
            Dominant dominantGroup dominantPct
                | dominantPct < classificationThreshold -> Unclassified
                | any isLiabilities creditAccounts && any isLiabilities debitAccounts ->
                    TransactionTransfer
                | any isLiabilities creditAccounts -> Borrowing
                | dominantGroup == Liabilities && any isLiabilities debitAccounts -> Repayment
                | dominantGroup == Assets
                    && any isCashOnly creditAccounts
                    && any isAssetOnly debitAccounts ->
                    AssetAcquisition
                | dominantGroup == Assets
                    && any isAssetOnly creditAccounts
                    && any isCashOnly debitAccounts ->
                    AssetDisposal
                | dominantGroup == Assets
                    && any isAssetOnly creditAccounts
                    && any isAssetOnly debitAccounts ->
                    AssetReallocation
                | dominantGroup == Assets && any isRevenue creditAccounts -> TransactionIncome
                | dominantGroup == Expenses -> TransactionExpense
                | otherwise -> TransactionTransfer
  where
    creditAccounts = postingAccountInfos (creditsFor tx.tpostings)
    debitAccounts = postingAccountInfos (debitsFor tx.tpostings)

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

dominantMovement :: Hledger.Transaction -> DominantMovement
dominantMovement tx
    | Map.null groupAmounts = NoDebits
    | otherwise = Dominant dominantGroup (Fraction (dominantAmount / totalDebit))
  where
    debitPs = debitsFor tx.tpostings
    groupAmounts =
        Map.fromListWith
            (+)
            [ ( accountGroup (accountInfo p.paccount)
              , abs (toRational amount.aquantity)
              )
            | p <- debitPs
            , amount <- Hledger.amountsRaw p.pamount
            , amount.aquantity > 0
            ]
    totalDebit = sum (Map.elems groupAmounts)
    (dominantGroup, dominantAmount) =
        Map.foldlWithKey'
            ( \(!bestGroup, !bestAmount) candidateGroup candidateAmount ->
                if candidateAmount > bestAmount
                    then (candidateGroup, candidateAmount)
                    else (bestGroup, bestAmount)
            )
            (Map.findMin groupAmounts)
            groupAmounts

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
