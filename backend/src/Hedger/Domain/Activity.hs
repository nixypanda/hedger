{- | Activity quantifies posting-level effects, bucketed over a period. A
transaction can contribute to more than one activity kind.
-}
module Hedger.Domain.Activity (
    ActivityKind (..),
    ActivityDirection (..),
    ActivityRange (..),
    BreakdownItem (..),
    AssetBucketFigures (..),
    LiabilityBucketFigures (..),
    IncomeBucketFigures (..),
    ExpenseBucketFigures (..),
    ActivityBuckets (..),
    ActivityFigures (..),
    ActivityEvent (..),
    AssetMovement (..),
    LiabilityMovement (..),
    PositiveAmount,
    FlowChange (..),
    ActivityIssue (..),
    computeActivity,
    parseActivityPeriod,
) where

import Data.Decimal (Decimal)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)
import Text.Read (readMaybe)

import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Account qualified as Account
import Hedger.Domain.ActivityEvent (
    ActivityEvent (..),
    ActivityIssue (..),
    AssetMovement (..),
    EventGroup (..),
    ExpenseAccount,
    FlowChange (..),
    IncomeAccount,
    LiabilityMovement (..),
    PositiveAmount,
    assetAccountName,
    classifyTransactionFor,
    expenseAccountName,
    incomeAccountName,
    liabilityAccountName,
    positiveAmountValue,
 )
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
    yearMonthMonth,
    yearMonthStart,
 )
import Hedger.Domain.Snapshot (Snapshot (..))

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
    { account :: Text
    , quantity :: Decimal
    }

data BucketProjection = BucketProjection
    { key :: Text
    , startDate :: Day
    , total :: Decimal
    , increase :: Decimal
    , decrease :: Decimal
    , reallocated :: Decimal
    , categoryItems :: [BreakdownItem]
    , directionItems :: [(ActivityDirection, [BreakdownItem])]
    , posted :: Decimal
    , reversed :: Decimal
    , forgiven :: Decimal
    , borrowingReversed :: Decimal
    , events :: [ActivityEvent]
    , transactions :: [Hledger.Transaction]
    }

data ActivityFigures = ActivityFigures
    { buckets :: ActivityBuckets
    , missing :: Set Text
    , issues :: Set ActivityIssue
    }

data AssetBucketFigures = AssetBucketFigures
    { key :: Text
    , startDate :: Day
    , acquired :: Decimal
    , disposed :: Decimal
    , transferred :: Decimal
    , categoryItems :: [BreakdownItem]
    , directionItems :: [(ActivityDirection, [BreakdownItem])]
    , movements :: [AssetMovement]
    , transactions :: [Hledger.Transaction]
    }

data LiabilityBucketFigures = LiabilityBucketFigures
    { key :: Text
    , startDate :: Day
    , borrowed :: Decimal
    , repaid :: Decimal
    , forgiven :: Decimal
    , borrowingReversed :: Decimal
    , categoryItems :: [BreakdownItem]
    , directionItems :: [(ActivityDirection, [BreakdownItem])]
    , movements :: [LiabilityMovement]
    , transactions :: [Hledger.Transaction]
    }

data IncomeBucketFigures = IncomeBucketFigures
    { key :: Text
    , startDate :: Day
    , posted :: Decimal
    , reversed :: Decimal
    , categoryItems :: [BreakdownItem]
    , changes :: [(IncomeAccount, FlowChange)]
    , transactions :: [Hledger.Transaction]
    }

data ExpenseBucketFigures = ExpenseBucketFigures
    { key :: Text
    , startDate :: Day
    , posted :: Decimal
    , reversed :: Decimal
    , categoryItems :: [BreakdownItem]
    , changes :: [(ExpenseAccount, FlowChange)]
    , transactions :: [Hledger.Transaction]
    }

data ActivityBuckets
    = AssetBuckets [AssetBucketFigures]
    | LiabilityBuckets [LiabilityBucketFigures]
    | IncomeBuckets [IncomeBucketFigures]
    | ExpenseBuckets [ExpenseBucketFigures]

computeActivity ::
    Snapshot ->
    [Hledger.Transaction] ->
    Text ->
    ActivityKind ->
    ActivityRange ->
    Account.DisplayDepth ->
    ReportPeriod ->
    ActivityFigures
computeActivity snapshot filtered commodity activityKind activityRange depth activityPeriod =
    ActivityFigures
        { buckets = typedBuckets activityKind projections
        , missing = Set.unions [missingCommodities | (_, missingCommodities, _) <- results]
        , issues = Set.unions [problems | (_, _, problems) <- results]
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
    contributions = mapMaybe (\(contribution, _, _) -> contribution) results
    bucketed =
        Map.fromListWith
            (<>)
            [ (activityBucketKey activityRange contribution.date, [contribution])
            | contribution <- contributions
            ]
    projections = fmap (toBucketFigures depth activityRange) (Map.toAscList bucketed)

typedBuckets :: ActivityKind -> [BucketProjection] -> ActivityBuckets
typedBuckets kind projections =
    case kind of
        AssetActivity -> AssetBuckets (fmap assetBucket projections)
        LiabilityActivity -> LiabilityBuckets (fmap liabilityBucket projections)
        IncomeActivity -> IncomeBuckets (fmap incomeBucket projections)
        ExpenseActivity -> ExpenseBuckets (fmap expenseBucket projections)

assetBucket :: BucketProjection -> AssetBucketFigures
assetBucket projection =
    AssetBucketFigures
        { key = projection.key
        , startDate = projection.startDate
        , acquired = projection.increase
        , disposed = projection.decrease
        , transferred = projection.reallocated
        , categoryItems = projection.categoryItems
        , directionItems = projection.directionItems
        , movements = [movement | AssetEvent movement <- projection.events]
        , transactions = projection.transactions
        }

liabilityBucket :: BucketProjection -> LiabilityBucketFigures
liabilityBucket projection =
    LiabilityBucketFigures
        { key = projection.key
        , startDate = projection.startDate
        , borrowed = projection.increase
        , repaid = projection.decrease - projection.forgiven - projection.borrowingReversed
        , forgiven = projection.forgiven
        , borrowingReversed = projection.borrowingReversed
        , categoryItems = projection.categoryItems
        , directionItems = projection.directionItems
        , movements = [movement | LiabilityEvent movement <- projection.events]
        , transactions = projection.transactions
        }

incomeBucket :: BucketProjection -> IncomeBucketFigures
incomeBucket projection =
    IncomeBucketFigures
        { key = projection.key
        , startDate = projection.startDate
        , posted = projection.posted
        , reversed = projection.reversed
        , categoryItems = projection.categoryItems
        , changes = [(account, change) | IncomeEvent account change <- projection.events]
        , transactions = projection.transactions
        }

expenseBucket :: BucketProjection -> ExpenseBucketFigures
expenseBucket projection =
    ExpenseBucketFigures
        { key = projection.key
        , startDate = projection.startDate
        , posted = projection.posted
        , reversed = projection.reversed
        , categoryItems = projection.categoryItems
        , changes = [(account, change) | ExpenseEvent account change <- projection.events]
        , transactions = projection.transactions
        }

toBucketFigures ::
    Account.DisplayDepth ->
    ActivityRange ->
    (Text, [ActivityContribution]) ->
    BucketProjection
toBucketFigures depth activityRange (key, contributions) =
    BucketProjection
        { key
        , startDate
        , total = sum (fmap (.total) contributions)
        , increase = sum (fmap (.increase) contributions)
        , decrease = sum (fmap (.decrease) contributions)
        , reallocated = sum (fmap (.reallocated) contributions)
        , categoryItems = itemsFrom movements
        , directionItems = directionItems
        , posted = sum (fmap (.posted) contributions)
        , reversed = sum (fmap (.reversed) contributions)
        , forgiven = sum (fmap (.forgiven) contributions)
        , borrowingReversed = sum (fmap (.borrowingReversed) contributions)
        , events = concatMap (.events) contributions
        , transactions = fmap (.transaction) contributions
        }
  where
    movements = concatMap (.categories) contributions
    startDate =
        case activityRange of
            MonthActivity -> fromMaybe epoch (readMaybe (Text.unpack key))
            FiscalYearActivity ->
                maybe epoch yearMonthStart (parseYearMonth key)
    itemsFrom ms =
        [ BreakdownItem{account, quantity = abs quantity}
        | (account, quantity) <-
            Map.toList
                (Map.fromListWith (+) [(Account.atDepth depth m.account, m.quantity) | m <- ms])
        ]
    directionItems =
        [ (direction, items)
        | direction <- [minBound .. maxBound]
        , let items = itemsFrom (concatMap (categoriesFor direction) contributions)
        , not (null items)
        ]
    categoriesFor direction contribution =
        Map.findWithDefault [] direction contribution.directionCategories

data ActivityContribution = ActivityContribution
    { date :: Day
    , total :: Decimal
    , increase :: Decimal
    , decrease :: Decimal
    , reallocated :: Decimal
    , posted :: Decimal
    , reversed :: Decimal
    , forgiven :: Decimal
    , borrowingReversed :: Decimal
    , categories :: [Movement]
    , directionCategories :: Map.Map ActivityDirection [Movement]
    , events :: [ActivityEvent]
    , transaction :: Hledger.Transaction
    }

activityContribution ::
    Snapshot ->
    Text ->
    ActivityKind ->
    Hledger.Transaction ->
    (Maybe ActivityContribution, Set Text, Set ActivityIssue)
activityContribution snapshot commodity activityKind transaction =
    ( if null relevant || not (Set.null missingCommodities)
        then Nothing
        else Just contribution
    , missingCommodities
    , problems
    )
  where
    (relevant, missingCommodities, problems) =
        classifyTransactionFor (eventGroup activityKind) snapshot commodity transaction
    parts = fmap eventPart relevant
    contribution =
        ActivityContribution
            { date = transaction.tdate
            , total = sum (fmap (.partTotal) parts)
            , increase = sum (fmap (.partIncrease) parts)
            , decrease = sum (fmap (.partDecrease) parts)
            , reallocated = sum (fmap (.partReallocated) parts)
            , posted = sum (fmap (.partPosted) parts)
            , reversed = sum (fmap (.partReversed) parts)
            , forgiven = sum (fmap (.partForgiven) parts)
            , borrowingReversed = sum (fmap (.partBorrowingReversed) parts)
            , categories = concatMap (.partCategories) parts
            , directionCategories = Map.unionsWith (<>) (fmap (.partDirectionCategories) parts)
            , events = relevant
            , transaction
            }

eventGroup :: ActivityKind -> EventGroup
eventGroup kind =
    case kind of
        AssetActivity -> AssetGroup
        LiabilityActivity -> LiabilityGroup
        IncomeActivity -> IncomeGroup
        ExpenseActivity -> ExpenseGroup

data EventPart = EventPart
    { partTotal :: Decimal
    , partIncrease :: Decimal
    , partDecrease :: Decimal
    , partReallocated :: Decimal
    , partPosted :: Decimal
    , partReversed :: Decimal
    , partForgiven :: Decimal
    , partBorrowingReversed :: Decimal
    , partCategories :: [Movement]
    , partDirectionCategories :: Map.Map ActivityDirection [Movement]
    }

eventPart :: ActivityEvent -> EventPart
eventPart event =
    case event of
        AssetEvent movement ->
            case movement of
                Acquired account amount -> directional Increase (assetAccountName account) amount
                Disposed account amount -> directional Decrease (assetAccountName account) amount
                Transferred source outgoing destination incoming ->
                    let sourceMovement =
                            Movement (assetAccountName source) (positiveAmountValue outgoing)
                        destinationMovement =
                            Movement (assetAccountName destination) (positiveAmountValue incoming)
                     in (emptyPart (positiveAmountValue outgoing))
                            { partReallocated = positiveAmountValue outgoing
                            , partCategories = [sourceMovement, destinationMovement]
                            , partDirectionCategories =
                                Map.singleton Reallocate [sourceMovement, destinationMovement]
                            }
        LiabilityEvent movement ->
            case movement of
                Borrowed account amount ->
                    directional Increase (liabilityAccountName account) amount
                Repaid account amount -> directional Decrease (liabilityAccountName account) amount
                Refinanced old repaid new borrowed ->
                    let oldMovement =
                            Movement (liabilityAccountName old) (positiveAmountValue repaid)
                        newMovement =
                            Movement (liabilityAccountName new) (positiveAmountValue borrowed)
                     in (emptyPart (positiveAmountValue repaid + positiveAmountValue borrowed))
                            { partIncrease = positiveAmountValue borrowed
                            , partDecrease = positiveAmountValue repaid
                            , partCategories = [oldMovement, newMovement]
                            , partDirectionCategories =
                                Map.fromList [(Increase, [newMovement]), (Decrease, [oldMovement])]
                            }
                Forgiven account amount ->
                    (directional Decrease (liabilityAccountName account) amount)
                        { partForgiven = positiveAmountValue amount
                        }
                BorrowingReversed account amount ->
                    (directional Decrease (liabilityAccountName account) amount)
                        { partBorrowingReversed = positiveAmountValue amount
                        }
        IncomeEvent account change -> flowPart (incomeAccountName account) change
        ExpenseEvent account change -> flowPart (expenseAccountName account) change
  where
    directional :: ActivityDirection -> Text -> PositiveAmount -> EventPart
    directional direction account amount =
        let quantity = positiveAmountValue amount
            movement = Movement account quantity
         in (emptyPart quantity)
                { partIncrease = if direction == Increase then quantity else 0
                , partDecrease = if direction == Decrease then quantity else 0
                , partCategories = [movement]
                , partDirectionCategories = Map.singleton direction [movement]
                }
    flowPart :: Text -> FlowChange -> EventPart
    flowPart account change =
        case change of
            Posted amount ->
                (emptyPart (positiveAmountValue amount))
                    { partPosted = positiveAmountValue amount
                    , partCategories = [Movement account (positiveAmountValue amount)]
                    }
            Reversed amount ->
                (emptyPart (negate (positiveAmountValue amount)))
                    { partReversed = positiveAmountValue amount
                    , partCategories = [Movement account (negate (positiveAmountValue amount))]
                    }

emptyPart :: Decimal -> EventPart
emptyPart total =
    EventPart
        { partTotal = total
        , partIncrease = 0
        , partDecrease = 0
        , partReallocated = 0
        , partPosted = 0
        , partReversed = 0
        , partForgiven = 0
        , partBorrowingReversed = 0
        , partCategories = []
        , partDirectionCategories = Map.empty
        }

activityBucketKey :: ActivityRange -> Day -> Text
activityBucketKey MonthActivity = dayKey
activityBucketKey FiscalYearActivity = monthKey

parseActivityPeriod :: Int -> ActivityRange -> Text -> Either AppError ReportPeriod
parseActivityPeriod fiscalStart activityRange raw =
    case activityRange of
        MonthActivity ->
            case parseYearMonth raw of
                Just yearMonth -> Right (monthPeriod (yearMonthStart yearMonth))
                Nothing -> Left (InvalidRequest "Activity month must use YYYY-MM.")
        FiscalYearActivity ->
            case parseYearMonth raw of
                Just yearMonth
                    | yearMonthMonth yearMonth == fiscalStart ->
                        Right (fiscalYearPeriod fiscalStart (yearMonthStart yearMonth))
                _ ->
                    Left
                        ( InvalidRequest
                            ( "Fiscal activity period must be the configured fiscal-year start "
                                <> "month in YYYY-MM form."
                            )
                        )
