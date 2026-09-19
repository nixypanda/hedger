module Hedger.Domain.Valuation (
    -- * Projecting amounts into a display commodity
    Conversion (..),
    valueMixedAt,
    conversionFromMixed,
    convertMixedAt,
    requireComplete,
    foldMapConversion,
    complete,
    completeNothing,
    completeDecimal,
    missingCommodities,
    ConversionWarning (..),
) where

import Data.Decimal (Decimal)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (Day)

import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Account qualified as Account
import Hedger.Domain.Snapshot (Snapshot (..))

{- | A value projected into the display commodity together with the set of
commodities that could not be converted (because a dated price was missing).
-}
data Conversion value = Conversion
    { value :: Maybe value
    , missing :: Set Text
    }
    deriving stock (Show)

instance Functor Conversion where
    fmap change conversion =
        Conversion{value = fmap change conversion.value, missing = conversion.missing}

-- | Value a mixed amount into the display commodity as at a given date.
valueMixedAt :: Snapshot -> Text -> Day -> Hledger.MixedAmount -> Hledger.MixedAmount
valueMixedAt snapshot commodity day =
    Account.valueAt (Account.mkValuationContext snapshot.journal commodity day)

{- | Sum the display-commodity components of an already-valued amount, recording
any non-negligible foreign commodities as missing.
-}
conversionFromMixed :: Text -> Hledger.MixedAmount -> Conversion Decimal
conversionFromMixed commodity amount =
    let projected = HledgerAmount.amountsRaw amount
        missing =
            Set.fromList
                [ item.acommodity
                | item <- projected
                , abs item.aquantity >= negligibleQuantity
                , item.acommodity /= commodity
                ]
     in Conversion{value = Just (commodityQuantity commodity amount), missing}

-- | Value an amount at a date and require a complete conversion for a result.
convertMixedAt :: Snapshot -> Text -> Day -> Hledger.MixedAmount -> Conversion Decimal
convertMixedAt snapshot commodity day amount =
    let valued = valueMixedAt snapshot commodity day amount
        projected = HledgerAmount.amountsRaw valued
        missing =
            Set.fromList
                [ item.acommodity
                | item <- projected
                , abs item.aquantity >= negligibleQuantity
                , item.acommodity /= commodity
                ]
     in Conversion
            { value =
                if Set.null missing
                    then Just (commodityQuantity commodity valued)
                    else Nothing
            , missing
            }

requireComplete :: Conversion value -> Conversion value
requireComplete conversion
    | Set.null conversion.missing = conversion
    | otherwise = Conversion{value = Nothing, missing = conversion.missing}

foldMapConversion :: [Conversion value] -> Conversion [value]
foldMapConversion conversions =
    let missing = Set.unions (fmap (.missing) conversions)
     in Conversion{value = Just (mapMaybe (.value) conversions), missing}

complete :: value -> Conversion value
complete value = Conversion{value = Just value, missing = Set.empty}

completeNothing :: Conversion value
completeNothing = Conversion{value = Nothing, missing = Set.empty}

{- | The display-commodity total of an amount, or 'Nothing' if any non-negligible
component is in another commodity.
-}
completeDecimal :: Text -> Hledger.MixedAmount -> Maybe Decimal
completeDecimal commodity amount
    | Set.null missing = Just displayQuantity
    | otherwise = Nothing
  where
    projected =
        filter ((>= negligibleQuantity) . abs . (.aquantity)) (HledgerAmount.amountsRaw amount)
    missing = Set.fromList [item.acommodity | item <- projected, item.acommodity /= commodity]
    displayQuantity =
        sum
            [ item.aquantity
            | item <-
                HledgerAmount.amountsRaw
                    (HledgerAmount.filterMixedAmountByCommodity commodity amount)
            , abs item.aquantity >= negligibleQuantity
            ]

commodityQuantity :: Text -> Hledger.MixedAmount -> Decimal
commodityQuantity commodity =
    sum
        . fmap (.aquantity)
        . HledgerAmount.amountsRaw
        . HledgerAmount.filterMixedAmountByCommodity commodity

-- | Quantities smaller than this are treated as rounding dust, not a real balance.
negligibleQuantity :: Decimal
negligibleQuantity = 0.000001

{- | A structured data-quality condition: the display-commodity conversion was
incomplete for these commodities. Rendering to prose is a boundary concern.
-}
newtype ConversionWarning
    = MissingConversion (Set Text)
    deriving stock (Eq, Show)

-- | The non-negligible commodities in an amount other than the display commodity.
missingCommodities :: Text -> Hledger.MixedAmount -> Set Text
missingCommodities commodity amount =
    Set.fromList
        [ item.acommodity
        | item <- HledgerAmount.amountsRaw amount
        , abs item.aquantity >= negligibleQuantity
        , item.acommodity /= commodity
        ]
