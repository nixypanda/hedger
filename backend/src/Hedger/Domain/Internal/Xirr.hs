{- | Generic XIRR calculation over externally supplied cash flows.

Amounts use the investor perspective: contributions are negative and proceeds
are positive.  The returned annual rate is a fraction, so @0.1@ means 10%.
-}
module Hedger.Domain.Internal.Xirr (
    AnnualRate,
    CashFlow,
    XirrError (..),
    annualRate,
    cashFlow,
    cashFlowAmount,
    cashFlowDate,
    xirr,
)
where

import Data.Decimal (Decimal, decimalMantissa, decimalPlaces, realFracToDecimal)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Time (Day, diffDays)
import Numeric.RootFinding qualified as RootFinding

newtype CashFlow = CashFlow (Day, Decimal)
    deriving stock (Eq, Show)

newtype AnnualRate = AnnualRate Decimal
    deriving stock (Eq, Show)

data XirrError
    = InsufficientCashFlows
    | CashFlowsDoNotChangeSign
    | CashFlowOutsideNumericalRange
    | NoConvergentRate
    | AmbiguousRates
    deriving stock (Eq, Show)

cashFlow :: Day -> Decimal -> CashFlow
cashFlow day amount = CashFlow (day, amount)

cashFlowDate :: CashFlow -> Day
cashFlowDate (CashFlow (day, _)) = day

cashFlowAmount :: CashFlow -> Decimal
cashFlowAmount (CashFlow (_, amount)) = amount

annualRate :: AnnualRate -> Decimal
annualRate (AnnualRate rate) = rate

{- | Calculate an annualised internal rate of return using Actual/365 fixed.

The root search works in @log (1 + rate)@ space.  This guarantees rates stay
strictly greater than -100% while allowing a broad, deterministic search
interval of -99.9999% through 100,000,000%.  Sign-changing roots found in that
interval are refined with Ridders' bracketed method.  More than one distinct
root is reported rather than chosen arbitrarily.
-}
xirr :: [CashFlow] -> Either XirrError AnnualRate
xirr cashFlows = do
    flows <- numericalCashFlows (normalise cashFlows)
    validateCashFlows flows
    case distinctRoots (rootCandidates (npvTerms flows)) of
        [] -> Left NoConvergentRate
        [root] ->
            let rate = exp root - 1
             in if finite rate
                    then Right (AnnualRate (realFracToDecimal 12 rate))
                    else Left NoConvergentRate
        _ -> Left AmbiguousRates

type NumericalCashFlow = (Day, Double)

normalise :: [CashFlow] -> [CashFlow]
normalise cashFlows = case normaliseChronological cashFlows of
    Just flows -> flows
    Nothing -> normaliseUnordered cashFlows

normaliseChronological :: [CashFlow] -> Maybe [CashFlow]
normaliseChronological [] = Just []
normaliseChronological (CashFlow (firstDay, firstAmount) : remaining) =
    fmap reverse (go firstDay firstAmount remaining [])
  where
    go day amount [] normalised = Just (addFlow day amount normalised)
    go day amount (CashFlow (nextDay, nextAmount) : flows) normalised
        | nextDay < day = Nothing
        | nextDay == day = go day (nextAmount + amount) flows normalised
        | otherwise = go nextDay nextAmount flows (addFlow day amount normalised)

    addFlow day amount normalised
        | amount == 0 = normalised
        | otherwise = cashFlow day amount : normalised

normaliseUnordered :: [CashFlow] -> [CashFlow]
normaliseUnordered cashFlows =
    [ cashFlow day amount
    | (day, amount) <- Map.toAscList grouped
    , amount /= 0
    ]
  where
    grouped = Map.fromListWith (+) [(cashFlowDate flow, cashFlowAmount flow) | flow <- cashFlows]

numericalCashFlows :: [CashFlow] -> Either XirrError [NumericalCashFlow]
numericalCashFlows = traverse toNumerical
  where
    toNumerical flow =
        case decimalToDouble (cashFlowAmount flow) of
            Just amount -> Right (cashFlowDate flow, amount)
            Nothing -> Left CashFlowOutsideNumericalRange

decimalToDouble :: Decimal -> Maybe Double
decimalToDouble decimal =
    let places = fromIntegral (decimalPlaces decimal) :: Int
        amount = (fromInteger (decimalMantissa decimal) :: Double) / ((10 :: Double) ^ places)
     in if finite amount then Just amount else Nothing

validateCashFlows :: [NumericalCashFlow] -> Either XirrError ()
validateCashFlows flows
    | length flows < 2 = Left InsufficientCashFlows
    | not (any ((< 0) . snd) flows && any ((> 0) . snd) flows) = Left CashFlowsDoNotChangeSign
    | otherwise = Right ()

rootCandidates :: NpvTerms -> [Double]
rootCandidates terms = concatMap solve (brackets terms)
  where
    solve (left, right) = case (snd left, snd right) of
        (0, _) -> [fst left]
        (_, 0) -> [fst right]
        _ -> case RootFinding.ridders riddersParameters (fst left, fst right) (npv terms) of
            RootFinding.Root root
                | finite root -> [root]
            _ -> []

brackets :: NpvTerms -> [((Double, Double), (Double, Double))]
brackets terms =
    [ (left, right)
    | (left, right) <- zip values (drop 1 values)
    , snd left == 0 || snd right == 0 || oppositeSigns (snd left) (snd right)
    ]
  where
    values = [(point, npv terms point) | point <- searchPoints]

searchPoints :: [Double]
searchPoints =
    [ minimumLogRate + (maximumLogRate - minimumLogRate) * fromIntegral index / fromIntegral searchIntervals
    | index <- [0 .. searchIntervals]
    ]

minimumLogRate :: Double
minimumLogRate = log 0.000001

maximumLogRate :: Double
maximumLogRate = log 1000001

searchIntervals :: Int
searchIntervals = 1024

riddersParameters :: RootFinding.RiddersParam
riddersParameters =
    RootFinding.RiddersParam
        { RootFinding.riddersMaxIter = 100
        , RootFinding.riddersTol = RootFinding.AbsTol 0.000000000001
        }

{- | Cash flows reduced to the only two numbers 'npv' needs per flow: the year
fraction since the first flow, and the amount scaled by the largest flow.
'brackets' evaluates the same flows at every one of 'searchPoints', so the date
arithmetic and scaling are done once here instead of once per rate.
-}
data NpvTerms = NpvTerms
    { terms :: [(Double, Double)]
    , lastFraction :: Double
    }

npvTerms :: [NumericalCashFlow] -> NpvTerms
npvTerms [] = NpvTerms{terms = [], lastFraction = 0}
npvTerms flows@((firstDay, _) : _) =
    NpvTerms
        { terms = [(yearFraction day, amount / scale) | (day, amount) <- flows]
        , lastFraction = yearFraction lastDay
        }
  where
    lastDay = fst (List.foldl' (\_ latest -> latest) (firstDay, 0) flows)
    yearFraction day = fromIntegral (diffDays day firstDay) / 365
    scale = maximum (fmap (abs . snd) flows)

{- | A scaled NPV.  Scaling by the largest cash flow and by a date-dependent
positive factor keeps evaluation finite without changing the locations of
zeros.
-}
npv :: NpvTerms -> Double -> Double
npv prepared logRate =
    sum
        [ amount * exp (negate logRate * (fraction - reference))
        | (fraction, amount) <- prepared.terms
        ]
  where
    reference
        | logRate < 0 = prepared.lastFraction
        | otherwise = 0

distinctRoots :: [Double] -> [Double]
distinctRoots = List.foldl' appendDistinct [] . List.sort
  where
    appendDistinct [] value = [value]
    appendDistinct accepted value
        | closeTo value (last accepted) = accepted
        | otherwise = accepted <> [value]
    closeTo left right = abs (left - right) <= 0.00000001

oppositeSigns :: Double -> Double -> Bool
oppositeSigns left right = (left < 0 && right > 0) || (left > 0 && right < 0)

finite :: Double -> Bool
finite value = not (isNaN value || isInfinite value)
