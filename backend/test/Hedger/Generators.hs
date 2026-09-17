module Hedger.Generators (
    genTransaction,
    genTransactions,

    -- * Amounts and commodities
    displayCommodity,
    otherCommodities,
    genCommodity,
    genQuantity,
    genAmount,
    genMixedAmount,

    -- * Calendar
    genYear,
    genMonth,
    genDay,
    formatYearMonth,

    -- * Accounts
    genAccountComponent,
    genQualifiedAccountName,

    -- * Investment mappings
    genPerformanceRole,
    genInvestmentMapping,
    genDisjointMappingSet,
) where

import Data.Decimal (Decimal, realFracToDecimal)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day (ModifiedJulianDay), fromGregorian)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Hledger.Data.Amount (mixed, num)
import Hledger.Data.Posting (nullposting)
import Hledger.Data.Transaction (nulltransaction)
import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Investment (
    InvestmentMapping (..),
    PerformanceAccount (..),
    PerformanceRole (..),
 )

genTransaction :: Gen Hledger.Transaction
genTransaction = do
    day <- ModifiedJulianDay <$> Gen.integral (Range.linear 40000 80000)
    index <- Gen.integral (Range.linear 0 100000)
    description <- genText (Range.linear 1 50)
    postings <- Gen.list (Range.linear 1 4) genPosting
    pure
        nulltransaction
            { Hledger.tdate = day
            , Hledger.tindex = index
            , Hledger.tdescription = description
            , Hledger.tpostings = postings
            }

genPosting :: Gen Hledger.Posting
genPosting = do
    account <- genText (Range.linear 1 30)
    amount <- Gen.integral (Range.linear (-1000000) 1000000)
    pure nullposting{Hledger.paccount = account, Hledger.pamount = mixed [num (fromInteger amount)]}

genTransactions :: Range.Range Int -> Gen [Hledger.Transaction]
genTransactions range = Gen.list range genTransaction

genText :: Range.Range Int -> Gen Text.Text
genText range = Text.pack <$> Gen.list range Gen.alphaNum

-- | The commodity that valuation tests project into.
displayCommodity :: Text
displayCommodity = "USD"

-- | Foreign commodities that valuation tests treat as unconvertible.
otherCommodities :: [Text]
otherCommodities = ["EUR", "GBP", "JPY"]

-- | Picks the display commodity or one of a small pool of foreign commodities.
genCommodity :: Gen Text
genCommodity = Gen.element (displayCommodity : otherCommodities)

{- | A quantity spanning both ordinary magnitudes and the tiny, near-zero
magnitudes that exercise negligible-amount thresholds.
-}
genQuantity :: Gen Decimal
genQuantity =
    Gen.choice
        [ realFracToDecimal 6 <$> Gen.double (Range.linearFrac (-1000000) 1000000)
        , realFracToDecimal 6 <$> Gen.double (Range.linearFrac (-0.000005) 0.000005)
        ]

genAmount :: Gen Hledger.Amount
genAmount = do
    commodity <- genCommodity
    quantity <- genQuantity
    pure (num quantity){Hledger.acommodity = commodity}

genMixedAmount :: Gen Hledger.MixedAmount
genMixedAmount = mixed <$> Gen.list (Range.linear 0 6) genAmount

-- Calendar
genYear :: Gen Integer
genYear = Gen.integral (Range.linear 1 9999)

genMonth :: Gen Int
genMonth = Gen.integral (Range.linear 1 12)

genDayOfMonth :: Gen Int
genDayOfMonth = Gen.integral (Range.linear 1 31)

-- | A valid calendar day; 'fromGregorian' clips an out-of-range day-of-month.
genDay :: Gen Day
genDay = fromGregorian <$> genYear <*> genMonth <*> genDayOfMonth

formatYearMonth :: Integer -> Int -> Text
formatYearMonth year month = Text.pack (show year <> "-" <> pad2 month)
  where
    pad2 value =
        if value < 10
            then "0" <> show value
            else show value

-- Accounts
genAccountComponent :: Gen Text
genAccountComponent = Text.pack <$> Gen.list (Range.linear 1 8) Gen.alphaNum

-- | A fully-qualified account name (at least two components) under a chosen top-level segment.
genQualifiedAccountName :: Text -> Gen Text
genQualifiedAccountName topSegment = do
    subs <- Gen.list (Range.linear 1 3) genAccountComponent
    pure (Text.intercalate ":" (topSegment : subs))

-- Investment mappings
genPerformanceRole :: Gen PerformanceRole
genPerformanceRole = Gen.element [Interest, Dividend, RealisedGain, Fee, OtherIncome]

performanceSegments :: [Text]
performanceSegments = ["income", "expenses", "liabilities", "equity"]

{- | A single, arbitrary investment mapping. Not guaranteed to be disjoint from any other
mapping; use 'genDisjointMappingSet' when a whole valid set is needed.
-}
genInvestmentMapping :: Gen InvestmentMapping
genInvestmentMapping = do
    assetAccount <- genQualifiedAccountName "assets"
    performanceAccounts <- Gen.list (Range.linear 0 3) genPerformanceAccount
    pure InvestmentMapping{assetAccount, performanceAccounts}
  where
    genPerformanceAccount = do
        segment <- Gen.element ("assets" : performanceSegments)
        account <- genQualifiedAccountName segment
        role <- genPerformanceRole
        pure PerformanceAccount{account, role}

{- | A set of investment mappings whose asset accounts are pairwise non-nested and whose
performance-account scopes are pairwise non-overlapping, so
'Hedger.Domain.Investment.mkInvestmentMappings'
accepts it.
-}
genDisjointMappingSet :: Gen [InvestmentMapping]
genDisjointMappingSet = do
    mappingCount <- Gen.integral (Range.linear 1 5)
    assetLeaves <- Set.toList <$> Gen.set (Range.singleton mappingCount) genAccountComponent
    counts <- Gen.list (Range.singleton mappingCount) (Gen.integral (Range.linear 0 3))
    performanceAccounts <- genDistinctPerformanceAccounts (sum counts)
    let performanceChunks = chunksByCounts counts performanceAccounts
    pure
        ( zipWith
            ( \leaf chunk ->
                InvestmentMapping{assetAccount = "assets:" <> leaf, performanceAccounts = chunk}
            )
            assetLeaves
            performanceChunks
        )

genDistinctPerformanceAccounts :: Int -> Gen [PerformanceAccount]
genDistinctPerformanceAccounts total = do
    leaves <- Set.toList <$> Gen.set (Range.singleton total) genAccountComponent
    segments <- Gen.list (Range.singleton total) (Gen.element performanceSegments)
    roles <- Gen.list (Range.singleton total) genPerformanceRole
    pure (zipWith3 toPerformanceAccount segments leaves roles)
  where
    toPerformanceAccount segment leaf role =
        PerformanceAccount{account = segment <> ":" <> leaf, role}

chunksByCounts :: [Int] -> [value] -> [[value]]
chunksByCounts [] _ = []
chunksByCounts (count : counts) values =
    let (chunk, rest) = splitAt count values
     in chunk : chunksByCounts counts rest
