{- | Posting-level activity classification. A transaction can produce several
effects, and the valued effects carry only accounts from their own group.
-}
module Hedger.Domain.ActivityEvent (
    PositiveAmount,
    positiveAmountValue,
    AssetAccount,
    assetAccountName,
    LiabilityAccount,
    liabilityAccountName,
    IncomeAccount,
    incomeAccountName,
    ExpenseAccount,
    expenseAccountName,
    FlowChange (..),
    AssetMovement (..),
    LiabilityMovement (..),
    ActivityEvent (..),
    ActivityIssue (..),
    EventGroup (..),
    classifyTransaction,
    classifyTransactionFor,
    activityLabelsFor,
) where

import Data.Decimal (Decimal)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Account qualified as Account
import Hedger.Domain.Snapshot (Snapshot)
import Hedger.Domain.Transaction qualified as Transaction
import Hedger.Domain.Valuation (Conversion (..), conversionMissing, convertMixedAt)

newtype PositiveAmount = PositiveAmount Decimal
    deriving stock (Eq, Show)

positiveAmountValue :: PositiveAmount -> Decimal
positiveAmountValue (PositiveAmount amount) = amount

positiveAmount :: Decimal -> Maybe PositiveAmount
positiveAmount amount
    | amount > 0.000001 = Just (PositiveAmount amount)
    | otherwise = Nothing

newtype AssetAccount = AssetAccount Text
    deriving stock (Eq, Show)

assetAccountName :: AssetAccount -> Text
assetAccountName (AssetAccount name) = name

newtype LiabilityAccount = LiabilityAccount Text
    deriving stock (Eq, Show)

liabilityAccountName :: LiabilityAccount -> Text
liabilityAccountName (LiabilityAccount name) = name

newtype IncomeAccount = IncomeAccount Text
    deriving stock (Eq, Show)

incomeAccountName :: IncomeAccount -> Text
incomeAccountName (IncomeAccount name) = name

newtype ExpenseAccount = ExpenseAccount Text
    deriving stock (Eq, Show)

expenseAccountName :: ExpenseAccount -> Text
expenseAccountName (ExpenseAccount name) = name

data FlowChange = Posted PositiveAmount | Reversed PositiveAmount
    deriving stock (Eq, Show)

data AssetMovement
    = Acquired AssetAccount PositiveAmount
    | Disposed AssetAccount PositiveAmount
    | Transferred AssetAccount PositiveAmount AssetAccount PositiveAmount
    deriving stock (Eq, Show)

data LiabilityMovement
    = Borrowed LiabilityAccount PositiveAmount
    | Repaid LiabilityAccount PositiveAmount
    | Refinanced LiabilityAccount PositiveAmount LiabilityAccount PositiveAmount
    | Forgiven LiabilityAccount PositiveAmount
    | BorrowingReversed LiabilityAccount PositiveAmount
    deriving stock (Eq, Show)

data ActivityEvent
    = AssetEvent AssetMovement
    | LiabilityEvent LiabilityMovement
    | IncomeEvent IncomeAccount FlowChange
    | ExpenseEvent ExpenseAccount FlowChange
    deriving stock (Eq, Show)

data ActivityIssue
    = AmbiguousAssetMovement
    | AmbiguousLiabilityMovement
    | UnsupportedAssetMovement
    | UnsupportedLiabilityMovement
    deriving stock (Eq, Ord, Show)

data EventGroup = AssetGroup | LiabilityGroup | IncomeGroup | ExpenseGroup
    deriving stock (Eq, Show)

data RawEvent
    = RawAcquired Hledger.Posting
    | RawDisposed Hledger.Posting
    | RawTransferred Hledger.Posting Hledger.Posting
    | RawBorrowed Hledger.Posting
    | RawRepaid Hledger.Posting
    | RawRefinanced Hledger.Posting Hledger.Posting
    | RawForgiven Hledger.Posting
    | RawBorrowingReversed Hledger.Posting
    | RawIncome Bool Hledger.Posting
    | RawExpense Bool Hledger.Posting

data PostingSide = Debit | Credit | Mixed | Zero
    deriving stock (Eq)

postingSide :: Hledger.Posting -> PostingSide
postingSide posting =
    case (any (> 0) amounts, any (< 0) amounts) of
        (True, False) -> Debit
        (False, True) -> Credit
        (True, True) -> Mixed
        (False, False) -> Zero
  where
    amounts = fmap (.aquantity) (HledgerAmount.amountsRaw posting.pamount)

isGroup :: Text -> Hledger.Posting -> Bool
isGroup group posting = Account.accountLabel (Account.accountInfo posting.paccount) == group

isCash :: Hledger.Posting -> Bool
isCash posting =
    Account.isCashOnly (Account.accountInfo posting.paccount)
        || any
            (`elem` ["bank", "cash", "checking", "current", "deposit", "savings", "wallet"])
            (fmap Text.toLower (Text.splitOn ":" posting.paccount))

rawEvents :: Hledger.Transaction -> ([RawEvent], Set ActivityIssue)
rawEvents transaction
    | Transaction.isOpening transaction = ([], Set.empty)
    | otherwise = (flows <> assetEvents <> liabilityEvents, assetIssues <> liabilityIssues)
  where
    postings = transaction.tpostings
    matching group side = filter (\p -> isGroup group p && postingSide p == side) postings
    assetIn = matching "assets" Debit
    assetOut = matching "assets" Credit
    debtIn = matching "liabilities" Credit
    debtOut = matching "liabilities" Debit
    incomeIn = matching "income" Credit
    expenseOut = matching "expenses" Credit
    cashOut = filter isCash assetOut
    flows =
        [RawIncome False p | p <- incomeIn]
            <> [RawIncome True p | p <- matching "income" Debit]
            <> [RawExpense False p | p <- matching "expenses" Debit]
            <> [RawExpense True p | p <- expenseOut]
    (assetEvents, assetIssues) =
        case (assetIn, assetOut) of
            ([incoming], [outgoing])
                | incoming.paccount == outgoing.paccount ->
                    ([], Set.singleton AmbiguousAssetMovement)
                | isCash outgoing && not (isCash incoming) -> ([RawAcquired incoming], Set.empty)
                | isCash incoming && not (isCash outgoing) -> ([RawDisposed outgoing], Set.empty)
                | otherwise -> ([RawTransferred outgoing incoming], Set.empty)
            ([incoming], [])
                | not (isCash incoming) && not (null debtIn) ->
                    ([RawAcquired incoming], Set.empty)
            ([], []) -> ([], Set.empty)
            ([incoming], []) ->
                ([], if isCash incoming then Set.empty else Set.singleton UnsupportedAssetMovement)
            ([], [outgoing]) ->
                ([], if isCash outgoing then Set.empty else Set.singleton UnsupportedAssetMovement)
            _ -> ([], Set.singleton AmbiguousAssetMovement)
    (liabilityEvents, liabilityIssues) =
        case (debtOut, debtIn) of
            ([outgoing], [incoming])
                | outgoing.paccount /= incoming.paccount ->
                    ([RawRefinanced outgoing incoming], Set.empty)
                | otherwise -> ([], Set.singleton AmbiguousLiabilityMovement)
            ([], credits) -> (fmap RawBorrowed credits, Set.empty)
            (debits, []) ->
                let classify debit
                        | not (null cashOut) = Just (RawRepaid debit)
                        | not (null incomeIn) = Just (RawForgiven debit)
                        | not (null expenseOut) = Just (RawBorrowingReversed debit)
                        | otherwise = Nothing
                    recognized = mapMaybe classify debits
                 in ( recognized
                    , if length recognized == length debits
                        then Set.empty
                        else Set.singleton UnsupportedLiabilityMovement
                    )
            _ -> ([], Set.singleton AmbiguousLiabilityMovement)

{- | Value each complete event at the transaction date. A missing conversion
never emits half of a linked transfer or refinance.
-}
classifyTransaction ::
    Snapshot -> Text -> Hledger.Transaction -> ([ActivityEvent], Set Text, Set ActivityIssue)
classifyTransaction = classifyTransactionWith (const True) (const True)

classifyTransactionFor ::
    EventGroup ->
    Snapshot ->
    Text ->
    Hledger.Transaction ->
    ([ActivityEvent], Set Text, Set ActivityIssue)
classifyTransactionFor group = classifyTransactionWith (belongs group) (issueBelongs group)

belongs :: EventGroup -> RawEvent -> Bool
belongs group event =
    case (group, event) of
        (AssetGroup, RawAcquired _) -> True
        (AssetGroup, RawDisposed _) -> True
        (AssetGroup, RawTransferred _ _) -> True
        (LiabilityGroup, RawBorrowed _) -> True
        (LiabilityGroup, RawRepaid _) -> True
        (LiabilityGroup, RawRefinanced _ _) -> True
        (LiabilityGroup, RawForgiven _) -> True
        (LiabilityGroup, RawBorrowingReversed _) -> True
        (IncomeGroup, RawIncome _ _) -> True
        (ExpenseGroup, RawExpense _ _) -> True
        _ -> False

issueBelongs :: EventGroup -> ActivityIssue -> Bool
issueBelongs group issue =
    case (group, issue) of
        (AssetGroup, AmbiguousAssetMovement) -> True
        (AssetGroup, UnsupportedAssetMovement) -> True
        (LiabilityGroup, AmbiguousLiabilityMovement) -> True
        (LiabilityGroup, UnsupportedLiabilityMovement) -> True
        _ -> False

classifyTransactionWith ::
    (RawEvent -> Bool) ->
    (ActivityIssue -> Bool) ->
    Snapshot ->
    Text ->
    Hledger.Transaction ->
    ([ActivityEvent], Set Text, Set ActivityIssue)
classifyTransactionWith include includeIssue snapshot commodity transaction =
    (mapMaybe valued results, Set.unions (fmap missing results), issues)
  where
    (raw, allIssues) = rawEvents transaction
    issues = Set.filter includeIssue allIssues
    results = fmap value (filter include raw)
    convert posting = convertMixedAt snapshot commodity transaction.tdate posting.pamount
    missing (_, missingCommodities) = missingCommodities
    valued (event, _) = event
    single constructor posting =
        let result = convert posting
         in (constructor <$> amountOf result, conversionMissing result)
    paired constructor source destination =
        let fromResult = convert source
            toResult = convert destination
            event = constructor <$> amountOf fromResult <*> amountOf toResult
         in (event, conversionMissing fromResult <> conversionMissing toResult)
    amountOf = \case
        Converted amount -> positiveAmount (abs amount)
        PartlyConverted _ _ -> Nothing
        Unconverted _ -> Nothing
        NotApplicable -> Nothing
    value = \case
        RawAcquired posting ->
            single (AssetEvent . Acquired (AssetAccount posting.paccount)) posting
        RawDisposed posting ->
            single (AssetEvent . Disposed (AssetAccount posting.paccount)) posting
        RawTransferred source destination ->
            paired
                ( \outgoing incoming ->
                    AssetEvent
                        ( Transferred
                            (AssetAccount source.paccount)
                            outgoing
                            (AssetAccount destination.paccount)
                            incoming
                        )
                )
                source
                destination
        RawBorrowed posting ->
            single (LiabilityEvent . Borrowed (LiabilityAccount posting.paccount)) posting
        RawRepaid posting ->
            single (LiabilityEvent . Repaid (LiabilityAccount posting.paccount)) posting
        RawRefinanced source destination ->
            paired
                ( \repaid borrowed ->
                    LiabilityEvent
                        ( Refinanced
                            (LiabilityAccount source.paccount)
                            repaid
                            (LiabilityAccount destination.paccount)
                            borrowed
                        )
                )
                source
                destination
        RawForgiven posting ->
            single (LiabilityEvent . Forgiven (LiabilityAccount posting.paccount)) posting
        RawBorrowingReversed posting ->
            single (LiabilityEvent . BorrowingReversed (LiabilityAccount posting.paccount)) posting
        RawIncome reversal posting ->
            single (IncomeEvent (IncomeAccount posting.paccount) . flow reversal) posting
        RawExpense reversal posting ->
            single (ExpenseEvent (ExpenseAccount posting.paccount) . flow reversal) posting
    flow reversal = if reversal then Reversed else Posted

-- | Stable labels for the transaction API; no display conversion is needed.
activityLabelsFor :: Hledger.Transaction -> [Text]
activityLabelsFor transaction = fmap label events
  where
    (events, _) = rawEvents transaction
    label = \case
        RawAcquired _ -> "asset_acquisition"
        RawDisposed _ -> "asset_disposal"
        RawTransferred _ _ -> "asset_transfer"
        RawBorrowed _ -> "borrowing"
        RawRepaid _ -> "repayment"
        RawRefinanced _ _ -> "refinancing"
        RawForgiven _ -> "debt_forgiveness"
        RawBorrowingReversed _ -> "borrowing_reversal"
        RawIncome True _ -> "income_reversal"
        RawIncome False _ -> "income"
        RawExpense True _ -> "expense_reversal"
        RawExpense False _ -> "expense"
