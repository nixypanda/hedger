module Hedger.Domain.Valuation (
    -- * Projecting amounts into a display commodity
    Conversion (..),
    valueMixedAt,
    conversionFromMixed,
    convertMixedAt,
    requireComplete,
    foldMapConversion,
    complete,
    notApplicable,
    conversionValue,
    conversionMissing,
    completeDecimal,
    missingCommodities,
    ConversionWarning (..),
) where

import Data.Decimal (Decimal)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (Day)

import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Account qualified as Account
import Hedger.Domain.Snapshot (Snapshot (..))

{- | The outcome of projecting an amount into the display commodity.

'Converted' and 'PartlyConverted' carry a value; 'Unconverted' and
'NotApplicable' carry none. Only the constructors that recorded unconvertible
commodities carry a non-empty missing set, so "carries a value together with an
empty missing set" is a complete conversion by construction.
-}
data Conversion value
    = Converted value
    | PartlyConverted value (NonEmpty Text)
    | Unconverted (NonEmpty Text)
    | NotApplicable
    deriving stock (Show)

instance Functor Conversion where
    fmap change = \case
        Converted value -> Converted (change value)
        PartlyConverted value missing -> PartlyConverted (change value) missing
        Unconverted missing -> Unconverted missing
        NotApplicable -> NotApplicable

{- | The projected value, when there is one. Present for 'Converted' and
'PartlyConverted', absent otherwise.
-}
conversionValue :: Conversion value -> Maybe value
conversionValue = \case
    Converted value -> Just value
    PartlyConverted value _ -> Just value
    Unconverted _ -> Nothing
    NotApplicable -> Nothing

-- | The commodities that could not be converted.
conversionMissing :: Conversion value -> Set Text
conversionMissing = \case
    Converted _ -> Set.empty
    PartlyConverted _ missing -> Set.fromList (NonEmpty.toList missing)
    Unconverted missing -> Set.fromList (NonEmpty.toList missing)
    NotApplicable -> Set.empty

-- | Value a mixed amount into the display commodity as at a given date.
valueMixedAt :: Snapshot -> Text -> Day -> Hledger.MixedAmount -> Hledger.MixedAmount
valueMixedAt snapshot commodity day =
    Account.valueAt (Account.mkValuationContext snapshot.journal commodity day)

{- | Sum the display-commodity components of an already-valued amount, recording
any non-negligible foreign commodities as missing.
-}
conversionFromMixed :: Text -> Hledger.MixedAmount -> Conversion Decimal
conversionFromMixed commodity amount =
    case NonEmpty.nonEmpty (Set.toList missing) of
        Nothing -> Converted total
        Just uncoverted -> PartlyConverted total uncoverted
  where
    total = commodityQuantity commodity amount
    missing =
        Set.fromList
            [ item.acommodity
            | item <- projected
            , abs item.aquantity >= negligibleQuantity
            , item.acommodity /= commodity
            ]
    projected = HledgerAmount.amountsRaw amount

-- | Value an amount at a date and require a complete conversion for a result.
convertMixedAt :: Snapshot -> Text -> Day -> Hledger.MixedAmount -> Conversion Decimal
convertMixedAt snapshot commodity day amount =
    case NonEmpty.nonEmpty (Set.toList missing) of
        Nothing -> Converted (commodityQuantity commodity valued)
        Just uncoverted -> Unconverted uncoverted
  where
    valued = valueMixedAt snapshot commodity day amount
    missing =
        Set.fromList
            [ item.acommodity
            | item <- projected
            , abs item.aquantity >= negligibleQuantity
            , item.acommodity /= commodity
            ]
    projected = HledgerAmount.amountsRaw valued

requireComplete :: Conversion value -> Conversion value
requireComplete = \case
    Converted value -> Converted value
    PartlyConverted _ missing -> Unconverted missing
    Unconverted missing -> Unconverted missing
    NotApplicable -> NotApplicable

foldMapConversion :: [Conversion value] -> Conversion [value]
foldMapConversion conversions =
    case NonEmpty.nonEmpty (Set.toList missing) of
        Nothing -> Converted values
        Just uncoverted -> PartlyConverted values uncoverted
  where
    missing = Set.unions (fmap conversionMissing conversions)
    values = mapMaybe conversionValue conversions

complete :: value -> Conversion value
complete = Converted

-- | This projection does not apply to the value; there is nothing to convert.
notApplicable :: Conversion value
notApplicable = NotApplicable

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
    = MissingConversion (NonEmpty Text)
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
