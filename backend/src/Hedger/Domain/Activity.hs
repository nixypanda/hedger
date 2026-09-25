{- | Activity quantifies movements into accounts, bucketed over a period. Income
and expense activity follows the corresponding postings because a transaction
can contain a flow alongside an asset or liability movement. Asset and liability
activity uses 'Hedger.Domain.Transaction' classification to determine direction.
-}
module Hedger.Domain.Activity (
    ActivityKind (..),
    ActivityDirection (..),
    ActivityRange (..),
    BreakdownItem (..),
    ActivityBucketFigures (..),
    ActivityFigures (..),
    computeActivity,
    parseActivityPeriod,
) where

import Data.Decimal (Decimal)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, fromGregorian)
import Text.Read (readMaybe)

import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Account qualified as Account
import Hedger.Domain.Error (AppError (..))
import Hedger.Domain.Internal.PeriodReport (BreakdownItem (..))
import Hedger.Domain.Period (
    ReportPeriod,
    dayKey,
    epoch,
    fiscalYearPeriod,
    monthKey,
    monthPeriod,
    parseYearMonth,
    reportPeriodEnd,
    reportPeriodStart,
 )
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Transaction qualified as Transaction
import Hedger.Domain.Valuation (
    Conversion (..),
    conversionMissing,
    conversionValue,
    convertMixedAt,
    foldMapConversion,
    notApplicable,
 )

data ActivityKind
    = IncomeActivity
    | ExpenseActivity
    | AssetActivity
    | LiabilityActivity
    deriving stock (Eq, Show)

data ActivityDirection
    = Increase
    | Reallocate
    | Decrease
    deriving stock (Eq, Ord, Show, Enum, Bounded)

data ActivityRange
    = MonthActivity
    | FiscalYearActivity
    deriving stock (Eq, Show)

data Movement = Movement
    { date :: Day
    , account :: Text
    , quantity :: Decimal
    }

data ActivityBucketFigures = ActivityBucketFigures
    { key :: Text
    , startDate :: Day
    , total :: Decimal
    , increase :: Decimal
    , decrease :: Decimal
    , reallocated :: Decimal
    , categoryItems :: [BreakdownItem]
    , directionItems :: [(ActivityDirection, [BreakdownItem])]
    , transactions :: [Hledger.Transaction]
    }

data ActivityFigures = ActivityFigures
    { buckets :: [ActivityBucketFigures]
    , missing :: Set Text
    }

computeActivity ::
    Snapshot ->
    [Hledger.Transaction] ->
    Text ->
    ActivityKind ->
    ActivityRange ->
    Int ->
    ReportPeriod ->
    ActivityFigures
computeActivity snapshot filtered commodity activityKind activityRange depth activityPeriod =
    ActivityFigures
        { buckets = fmap (toBucketFigures depth activityRange) (Map.toAscList bucketed)
        , missing = Set.unions (fmap conversionMissing results)
        }
  where
    inRange =
        filter
            ( \transaction ->
                transaction.tdate >= reportPeriodStart activityPeriod
                    && transaction.tdate <= reportPeriodEnd activityPeriod
            )
            filtered
    results = fmap (activityContribution snapshot commodity activityKind) inRange
    contributions = mapMaybe conversionValue results
    bucketed =
        Map.fromListWith
            (<>)
            [ (activityBucketKey activityRange contribution.date, [contribution])
            | contribution <- contributions
            ]

toBucketFigures :: Int -> ActivityRange -> (Text, [ActivityContribution]) -> ActivityBucketFigures
toBucketFigures depth activityRange (key, contributions) =
    ActivityBucketFigures
        { key
        , startDate
        , total = sum (fmap (.total) contributions)
        , increase = sum (fmap (.increase) contributions)
        , decrease = sum (fmap (.decrease) contributions)
        , reallocated = sum (fmap (.reallocated) contributions)
        , categoryItems = itemsFrom movements
        , directionItems = directionItems
        , transactions = fmap (.transaction) contributions
        }
  where
    movements = concatMap (.categories) contributions
    startDate =
        case activityRange of
            MonthActivity -> fromMaybe epoch (readMaybe (Text.unpack key))
            FiscalYearActivity ->
                maybe epoch (\(year, month) -> fromGregorian year month 1) (parseYearMonth key)
    itemsFrom ms =
        [ BreakdownItem{account, quantity = abs quantity}
        | (account, quantity) <-
            Map.toList
                (Map.fromListWith (+) [(Account.atDepth depth m.account, m.quantity) | m <- ms])
        ]
    directionItems =
        [ (direction, items)
        | direction <- [minBound .. maxBound]
        , let items = itemsFrom (concatMap (.categories) (contributionsFor direction))
        , not (null items)
        ]
    contributionsFor direction = filter ((== Just direction) . (.direction)) contributions

belongsTo :: Text -> Hledger.Posting -> Bool
belongsTo category posting = Account.accountLabel (Account.accountInfo posting.paccount) == category

categoryPostingMovements ::
    Snapshot ->
    Text ->
    Text ->
    Hledger.Transaction ->
    (Decimal -> Decimal) ->
    Conversion [Movement]
categoryPostingMovements snapshot commodity category transaction change =
    foldMapConversion
        [ fmap
            (Movement transaction.tdate posting.paccount . change)
            (convertMixedAt snapshot commodity transaction.tdate posting.pamount)
        | posting <- transaction.tpostings
        , belongsTo category posting
        ]

data ActivityContribution = ActivityContribution
    { date :: Day
    , total :: Decimal
    , increase :: Decimal
    , decrease :: Decimal
    , reallocated :: Decimal
    , direction :: Maybe ActivityDirection
    , categories :: [Movement]
    , transaction :: Hledger.Transaction
    }

directionFor :: Transaction.TransactionClassification -> Maybe ActivityDirection
directionFor classification =
    case classification of
        Transaction.AssetAcquisition -> Just Increase
        Transaction.Borrowing -> Just Increase
        Transaction.AssetReallocation -> Just Reallocate
        Transaction.AssetDisposal -> Just Decrease
        Transaction.Repayment -> Just Decrease
        _ -> Nothing

activityContribution ::
    Snapshot -> Text -> ActivityKind -> Hledger.Transaction -> Conversion ActivityContribution
activityContribution snapshot commodity activityKind transaction =
    case activityKind of
        IncomeActivity -> flowActivity snapshot commodity "income" negate transaction
        ExpenseActivity -> flowActivity snapshot commodity "expenses" id transaction
        AssetActivity ->
            case classification of
                Transaction.AssetAcquisition ->
                    assetActivity snapshot commodity classification transaction
                Transaction.AssetDisposal ->
                    assetActivity snapshot commodity classification transaction
                Transaction.AssetReallocation ->
                    assetActivity snapshot commodity classification transaction
                _ -> notApplicable
        LiabilityActivity ->
            case classification of
                Transaction.Borrowing ->
                    liabilityActivity snapshot commodity classification transaction
                Transaction.Repayment ->
                    liabilityActivity snapshot commodity classification transaction
                _ -> notApplicable
  where
    classification = Transaction.classificationFor transaction

flowActivity ::
    Snapshot ->
    Text ->
    Text ->
    (Decimal -> Decimal) ->
    Hledger.Transaction ->
    Conversion ActivityContribution
flowActivity snapshot commodity category change transaction =
    let movementsResult = categoryPostingMovements snapshot commodity category transaction change
        contribution = do
            movements <- conversionValue movementsResult
            let total = sum (fmap (.quantity) movements)
            if abs total < 0.000001
                then Nothing
                else
                    Just
                        ActivityContribution
                            { date = transaction.tdate
                            , total
                            , increase = 0
                            , decrease = 0
                            , reallocated = 0
                            , direction = Nothing
                            , categories = movements
                            , transaction
                            }
     in case NonEmpty.nonEmpty (Set.toList (conversionMissing movementsResult)) of
            Just unconverted -> Unconverted unconverted
            Nothing -> maybe NotApplicable Converted contribution

assetActivity ::
    Snapshot ->
    Text ->
    Transaction.TransactionClassification ->
    Hledger.Transaction ->
    Conversion ActivityContribution
assetActivity snapshot commodity classification transaction =
    let results =
            [ fmap
                (\amount -> (posting, amount))
                (convertMixedAt snapshot commodity transaction.tdate posting.pamount)
            | posting <- transaction.tpostings
            , belongsTo "assets" posting
            ]
        missing = Set.unions (fmap conversionMissing results)
        postings = mapMaybe conversionValue results
        cash posting =
            Account.isCashOnly (Account.accountInfo posting.paccount)
                || inferredCashName posting.paccount
        totalWhere predicate =
            sum [abs amount | (posting, amount) <- postings, predicate posting amount]
        otherIn = totalWhere (\posting amount -> not (cash posting) && amount > 0)
        otherOut = totalWhere (\posting amount -> not (cash posting) && amount < 0)
        (acquired, disposed, realloc) =
            case classification of
                Transaction.AssetAcquisition -> (otherIn, 0, 0)
                Transaction.AssetDisposal -> (0, otherOut, 0)
                Transaction.AssetReallocation -> (0, 0, min otherIn otherOut)
                _ -> (0, 0, 0)
        categoryValues =
            [ Movement transaction.tdate posting.paccount (abs amount)
            | (posting, amount) <- postings
            , not (cash posting)
            , abs amount >= 0.000001
            ]
        relevant = acquired > 0 || disposed > 0 || realloc > 0
     in case NonEmpty.nonEmpty (Set.toList missing) of
            Just unconverted -> Unconverted unconverted
            Nothing ->
                if relevant
                    then
                        Converted
                            ActivityContribution
                                { date = transaction.tdate
                                , total = acquired + disposed + realloc
                                , increase = acquired
                                , decrease = disposed
                                , reallocated = realloc
                                , direction = directionFor classification
                                , categories = categoryValues
                                , transaction
                                }
                    else NotApplicable

liabilityActivity ::
    Snapshot ->
    Text ->
    Transaction.TransactionClassification ->
    Hledger.Transaction ->
    Conversion ActivityContribution
liabilityActivity snapshot commodity classification transaction =
    let results =
            [ fmap
                (Movement transaction.tdate posting.paccount)
                (convertMixedAt snapshot commodity transaction.tdate posting.pamount)
            | posting <- transaction.tpostings
            , belongsTo "liabilities" posting
            ]
        missing = Set.unions (fmap conversionMissing results)
        movements = mapMaybe conversionValue results
        borrowed =
            if classification == Transaction.Borrowing
                then sum [abs movement.quantity | movement <- movements, movement.quantity < 0]
                else 0
        repaid =
            if classification == Transaction.Repayment
                then sum [movement.quantity | movement <- movements, movement.quantity > 0]
                else 0
        relevant = borrowed > 0 || repaid > 0
     in case NonEmpty.nonEmpty (Set.toList missing) of
            Just unconverted -> Unconverted unconverted
            Nothing ->
                if relevant
                    then
                        Converted
                            ActivityContribution
                                { date = transaction.tdate
                                , total = borrowed + repaid
                                , increase = borrowed
                                , decrease = repaid
                                , reallocated = 0
                                , direction = directionFor classification
                                , categories = fmap absoluteMovement movements
                                , transaction
                                }
                    else NotApplicable

inferredCashName :: Text -> Bool
inferredCashName account =
    any
        (`elem` ["bank", "cash", "checking", "current", "deposit", "savings", "wallet"])
        (fmap Text.toLower (Text.splitOn ":" account))

absoluteMovement :: Movement -> Movement
absoluteMovement movement =
    Movement{date = movement.date, account = movement.account, quantity = abs movement.quantity}

activityBucketKey :: ActivityRange -> Day -> Text
activityBucketKey MonthActivity = dayKey
activityBucketKey FiscalYearActivity = monthKey

parseActivityPeriod :: Int -> ActivityRange -> Text -> Either AppError ReportPeriod
parseActivityPeriod fiscalStart activityRange raw =
    case activityRange of
        MonthActivity ->
            case parseYearMonth raw of
                Just (year, month) -> Right (monthPeriod (fromGregorian year month 1))
                Nothing -> Left (InvalidRequest "Activity month must use YYYY-MM.")
        FiscalYearActivity ->
            case parseYearMonth raw of
                Just (year, month)
                    | month == fiscalStart ->
                        let first = fromGregorian year month 1
                         in Right (fiscalYearPeriod fiscalStart first)
                _ ->
                    Left
                        ( InvalidRequest
                            ( "Fiscal activity period must be the configured fiscal-year start "
                                <> "month in YYYY-MM form."
                            )
                        )
