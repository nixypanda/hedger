module Hedger.Generators (
    genTransaction,
    genTransactions,

    -- * Ledgers
    Intent (..),
    genIntentTransaction,
    genSnapshot,
    genPriceDirective,

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
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day (ModifiedJulianDay), addDays, fromGregorian)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Hledger.Data.Account qualified as HledgerAccount
import Hledger.Data.Amount (mixed, num)
import Hledger.Data.Journal (nulljournal)
import Hledger.Data.Posting (nullposting)
import Hledger.Data.Transaction (nulltransaction)
import Hledger.Data.Types qualified as Hledger

import Hedger.Domain.Investment (
    InvestmentMapping (..),
    PerformanceAccount (..),
    PerformanceRole (..),
 )
import Hedger.Domain.Snapshot (Snapshot (..))

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

-- Ledgers

{- | The intended classification of a generated transaction: the
'Hedger.Domain.Internal.Transaction.TransactionClassification' constructor
that 'genIntentTransaction' targets.
-}
data Intent
    = ExpenseIntent
    | IncomeIntent
    | TransferIntent
    | OpeningIntent
    | AcquisitionIntent
    | DisposalIntent
    | ReallocationIntent
    | BorrowingIntent
    | RepaymentIntent
    deriving stock (Eq, Show, Enum, Bounded)

-- | Account names whose inferred hledger types drive classification.
cashAccountNames :: [Text]
cashAccountNames = ["assets:cash:bank", "assets:checking", "assets:savings"]

nonCashAssetAccountNames :: [Text]
nonCashAssetAccountNames =
    ["assets:investments:stock", "assets:investments:fund", "assets:property:home"]

liabilityAccountNames :: [Text]
liabilityAccountNames = ["liabilities:loan:personal", "liabilities:card"]

incomeAccountNames :: [Text]
incomeAccountNames = ["income:salary", "income:capital:gains"]

expenseAccountNames :: [Text]
expenseAccountNames = ["expenses:food", "expenses:financial:interest"]

openingEquityAccountName :: Text
openingEquityAccountName = "equity:opening"

{- | Builds a small, balanced transaction whose account types make
'Hedger.Domain.Internal.Transaction.classificationFor' return the classification
implied by the argument.
-}
genIntentTransaction :: Intent -> Gen Hledger.Transaction
genIntentTransaction intent = do
    day <- genLedgerDate
    magnitude <- genMagnitude
    postings <- genIntentPostings intent magnitude
    pure
        nulltransaction
            { Hledger.tdate = day
            , Hledger.tpostings = postings
            }

-- | A date spanning roughly January 2024 through February 2025.
genLedgerDate :: Gen Day
genLedgerDate =
    addDays <$> Gen.integral (Range.linear 0 397) <*> pure (fromGregorian 2024 1 1)

-- | A strictly positive magnitude; callers apply the sign their recipe requires.
genMagnitude :: Gen Decimal
genMagnitude = fromInteger <$> Gen.integral (Range.linear 1 1000000)

-- | A posting for the display commodity.
intentPosting :: Text -> Decimal -> Hledger.Posting
intentPosting account quantity =
    nullposting
        { Hledger.paccount = account
        , Hledger.pamount = mixed [(num quantity){Hledger.acommodity = displayCommodity}]
        }

genIntentPostings :: Intent -> Decimal -> Gen [Hledger.Posting]
genIntentPostings intent magnitude = case intent of
    ExpenseIntent -> do
        expense <- Gen.element expenseAccountNames
        cash <- Gen.element cashAccountNames
        pure [intentPosting expense magnitude, intentPosting cash (negate magnitude)]
    IncomeIntent -> do
        income <- Gen.element incomeAccountNames
        cash <- Gen.element cashAccountNames
        pure [intentPosting income (negate magnitude), intentPosting cash magnitude]
    TransferIntent -> do
        (from, to) <- genTwoDistinct cashAccountNames
        pure [intentPosting from (negate magnitude), intentPosting to magnitude]
    OpeningIntent -> do
        cash <- Gen.element cashAccountNames
        pure
            [ intentPosting openingEquityAccountName (negate magnitude)
            , intentPosting cash magnitude
            ]
    AcquisitionIntent -> do
        cash <- Gen.element cashAccountNames
        asset <- Gen.element nonCashAssetAccountNames
        pure [intentPosting cash (negate magnitude), intentPosting asset magnitude]
    DisposalIntent -> do
        asset <- Gen.element nonCashAssetAccountNames
        cash <- Gen.element cashAccountNames
        pure [intentPosting asset (negate magnitude), intentPosting cash magnitude]
    ReallocationIntent -> do
        (from, to) <- genTwoDistinct nonCashAssetAccountNames
        pure [intentPosting from (negate magnitude), intentPosting to magnitude]
    BorrowingIntent -> do
        liability <- Gen.element liabilityAccountNames
        cash <- Gen.element cashAccountNames
        pure [intentPosting liability (negate magnitude), intentPosting cash magnitude]
    RepaymentIntent -> do
        cash <- Gen.element cashAccountNames
        liability <- Gen.element liabilityAccountNames
        pure [intentPosting cash (negate magnitude), intentPosting liability magnitude]

-- | Two distinct elements of a pool that has at least two members.
genTwoDistinct :: (Eq value) => [value] -> Gen (value, value)
genTwoDistinct values = do
    first <- Gen.element values
    second <- Gen.element (filter (/= first) values)
    pure (first, second)

{- | A whole ledger: a nonempty list of transactions drawn from a random
nonempty subset of 'Intent's (with repetitions), packaged as a 'Snapshot'.
-}
genSnapshot :: Gen Snapshot
genSnapshot = do
    intents <- Gen.list (Range.linear 1 24) (Gen.element [minBound .. maxBound])
    transactions <- traverse genIntentTransaction intents
    pure
        Snapshot
            { journal = nulljournal{Hledger.jtxns = transactions}
            , transactions = transactions
            , accountTree =
                HledgerAccount.accountFromPostings
                    (const Nothing)
                    (concatMap Hledger.tpostings transactions)
            , latestJournalDate = maximumMaybe (Hledger.tdate <$> transactions)
            , revision = "md5:synthetic"
            }

-- | Total maximum, unlike the partial 'maximum'.
maximumMaybe :: (Ord value) => [value] -> Maybe value
maximumMaybe [] = Nothing
maximumMaybe (value : values) = Just (List.foldl' max value values)

{- | A price directive stating that one unit of @symbol@ is worth @price@ units
of the display commodity.
-}
genPriceDirective :: Day -> Text -> Decimal -> Hledger.PriceDirective
genPriceDirective day symbol price =
    Hledger.PriceDirective
        { Hledger.pdsourcepos = Hledger.nullsourcepos
        , Hledger.pddate = day
        , Hledger.pdcommodity = symbol
        , Hledger.pdamount = (num price){Hledger.acommodity = displayCommodity}
        }

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
