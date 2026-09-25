module Hedger.Domain.Account (
    AccountInfo (..),
    DisplayDepth (..),
    ValuationContext,
    ValuationCompleteness (..),
    ValuedBalances (..),
    accountInfo,
    accountLabel,
    atDepth,
    displayDepthValue,
    isCashOnly,
    mkValuationContext,
    parseDisplayDepth,
    valueAt,
    valueBalances,
) where

import Data.Text (Text)
import Data.Time (Day)

import Hledger.Data.AccountName qualified as HledgerAccountName
import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Journal qualified as HledgerJournal
import Hledger.Data.Types qualified as Hledger
import Hledger.Data.Valuation qualified as HledgerValuation

import Hedger.Domain.Internal.Account

-- | Collapse an account path to the first @depth@ components.
atDepth :: DisplayDepth -> Text -> Text
atDepth depth =
    HledgerAccountName.clipAccountName (Hledger.DepthSpec (Just (displayDepthValue depth)) [])

data ValuationCompleteness
    = Complete
    | Partial
    | Unavailable
    deriving stock (Eq, Show)

data ValuedBalances = ValuedBalances
    { directBalance :: Hledger.MixedAmount
    , subtreeBalance :: Hledger.MixedAmount
    , completeness :: ValuationCompleteness
    }

data ValuationContext = ValuationContext
    { targetCommodity :: Text
    , valueAtDate :: Hledger.MixedAmount -> Hledger.MixedAmount
    }

mkValuationContext :: Hledger.Journal -> Text -> Day -> ValuationContext
mkValuationContext journal targetCommodity valuationDate =
    ValuationContext
        { targetCommodity
        , valueAtDate =
            HledgerValuation.mixedAmountValueAtDate
                (HledgerValuation.journalPriceOracle False journal)
                (HledgerJournal.journalCommodityStyles journal)
                (Just targetCommodity)
                valuationDate
        }

valueBalances :: ValuationContext -> Hledger.MixedAmount -> Hledger.MixedAmount -> ValuedBalances
valueBalances context direct subtree =
    ValuedBalances
        { directBalance = valuedDirect
        , subtreeBalance = valuedSubtree
        , completeness = valuationCompleteness context.targetCommodity valuedDirect valuedSubtree
        }
  where
    valuedDirect = context.valueAtDate direct
    valuedSubtree = context.valueAtDate subtree

valueAt :: ValuationContext -> Hledger.MixedAmount -> Hledger.MixedAmount
valueAt context = context.valueAtDate

valuationCompleteness :: Text -> Hledger.MixedAmount -> Hledger.MixedAmount -> ValuationCompleteness
valuationCompleteness target direct subtree
    | all isTarget projected = Complete
    | any isTarget projected = Partial
    | otherwise = Unavailable
  where
    projected = HledgerAmount.amountsRaw direct <> HledgerAmount.amountsRaw subtree
    isTarget amount = amount.acommodity == target
