module Main (main) where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (evaluate)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text qualified as Text
import Data.Time (Day, addDays, addGregorianMonthsClip, fromGregorian, toGregorian)
import Hedger.Application.TransactionQuery (
    TransactionPage (transactions),
    TransactionPageRequest (..),
    transactionPage,
 )
import Hedger.Domain.Investment (
    InvestmentMapping (..),
    PerformanceAccount (..),
    PerformanceRole (..),
    mkInvestmentMappings,
 )
import Hedger.Domain.Investment qualified as Investment
import Hedger.Domain.InvestmentPerformance qualified as Performance
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Xirr qualified as Xirr
import Hedger.Infrastructure.SnapshotStore (loadSnapshot)
import Hedger.Interface.HTTP.Common (ResponseMetadata (..))
import Hedger.Interface.HTTP.Transactions (
    AmountDto (..),
    CostDto (..),
    PostingDto (..),
    TagDto (..),
    TransactionDto (..),
    TransactionPresentationDto (..),
    TransactionsResponse (..),
    toTransactions,
 )
import Hledger.Data.Types qualified as Hledger
import Test.Tasty.Bench (Benchmark, bench, bgroup, defaultMain, env, nf, nfIO)

main :: IO ()
main =
    defaultMain
        [ bench "snapshot/parse-shared-13" $ nfIO loadAndForceFixture
        , queryBenchmarks "shared-13" "test/fixtures/shared.journal"
        , queryBenchmarks "large-50000" "../examples/performance/india-us-portfolio-large-50000.journal"
        , investmentBenchmarks "shared-13" "test/fixtures/shared.journal"
        , investmentBenchmarks "large-50000" "../examples/performance/india-us-portfolio-large-50000.journal"
        , bench "xirr/small" $ nf xirrWeight smallXirrCashFlows
        , xirrBenchmarks "shared-13" "test/fixtures/shared.journal"
        , xirrBenchmarks "large-50000" "../examples/performance/india-us-portfolio-large-50000.journal"
        , env loadTransactions $ \transactions ->
            bgroup
                "transactions"
                [ bench "project/shared-13" $ nf (projectionWeight . toTransactions) transactions
                , bench "project/scaled-1300" $
                    nf (projectionWeight . toTransactions) (concat (replicate 100 transactions))
                , bench "encode/shared-13" $
                    nf (LazyByteString.length . Aeson.encode . responseFor) transactions
                , bench "encode/scaled-1300" $
                    nf
                        (LazyByteString.length . Aeson.encode . responseFor)
                        (concat (replicate 100 transactions))
                ]
        ]

newtype BenchSnapshot = BenchSnapshot Snapshot

instance NFData BenchSnapshot where
    rnf (BenchSnapshot snapshot) = rnf (sum (length . Hledger.tpostings <$> snapshot.transactions))

newtype BenchXirrCashFlows = BenchXirrCashFlows [Xirr.CashFlow]

instance NFData BenchXirrCashFlows where
    rnf (BenchXirrCashFlows flows) = rnf (length (show flows))

data InvestmentInput = InvestmentInput
    { snapshot :: Snapshot
    , latest :: Day
    , monthEnds :: [Day]
    }

instance NFData InvestmentInput where
    rnf input =
        rnf (sum (length . Hledger.tpostings <$> input.snapshot.transactions)) `seq`
            rnf input.latest `seq`
                rnf input.monthEnds

queryBenchmarks :: String -> Text.Text -> Benchmark
queryBenchmarks fixtureName fixturePath =
    env (BenchSnapshot <$> loadFixtureAt fixturePath) $ \(BenchSnapshot snapshot) ->
        bgroup
            ("query/" <> fixtureName)
            [ bench "account" $ nf (queryWeight snapshot) "acct:assets"
            , bench "boolean" $ nf (queryWeight snapshot) "expr:'(acct:expenses OR desc:Salary)'"
            ]

investmentBenchmarks :: String -> Text.Text -> Benchmark
investmentBenchmarks fixtureName fixturePath =
    env (loadInvestmentInput fixturePath) $ \input ->
        bgroup
            ("investments/" <> fixtureName)
            [ bench "current" $
                nf
                    (summaryWeight . investmentSummary input.snapshot)
                    input.latest
            , bench "monthly-24" $
                nf
                    (sum . fmap (summaryWeight . investmentSummary input.snapshot))
                    input.monthEnds
            ]

xirrBenchmarks :: String -> Text.Text -> Benchmark
xirrBenchmarks fixtureName fixturePath =
    env (BenchXirrCashFlows <$> loadDerivedXirrCashFlows fixturePath) $ \(BenchXirrCashFlows flows) ->
        bgroup ("xirr/" <> fixtureName) [bench "journal-derived" $ nf xirrWeight flows]

xirrWeight :: [Xirr.CashFlow] -> Int
xirrWeight = length . show . Xirr.xirr

smallXirrCashFlows :: [Xirr.CashFlow]
smallXirrCashFlows =
    [ Xirr.cashFlow (fromGregorian 2024 1 1) (-1000)
    , Xirr.cashFlow (fromGregorian 2024 7 1) (-500)
    , Xirr.cashFlow (fromGregorian 2025 1 1) 1800
    ]

loadDerivedXirrCashFlows :: Text.Text -> IO [Xirr.CashFlow]
loadDerivedXirrCashFlows fixturePath = do
    snapshot <- loadFixtureAt fixturePath
    let dates = (.tdate) <$> snapshot.transactions
        flows = case dates of
            [] -> smallXirrCashFlows
            first : rest ->
                let contributionCount = length rest + 1
                    terminal = addDays 365 (maximum dates)
                 in Xirr.cashFlow first (-1)
                        : [Xirr.cashFlow day (-1) | day <- rest]
                            <> [Xirr.cashFlow terminal (fromIntegral (contributionCount + 1000))]
    _ <- evaluate (sum (length . show <$> flows))
    pure flows

investmentSummary :: Snapshot -> Day -> Performance.InvestmentSummary
investmentSummary snapshot day =
    Performance.investmentSummaryAt snapshot benchmarkMappings "INR" Nothing day

summaryWeight :: Performance.InvestmentSummary -> Int
summaryWeight summary =
    sum
        [ length (show summary.marketValue)
        , length (show summary.purchaseCost)
        , length (show summary.gainLoss)
        , length (show summary.xirr)
        , length (show summary.attributedIncome)
        , length (show summary.realisedGain)
        , length (show summary.totalProfit)
        , length (show summary.completeness)
        ]

benchmarkMappings :: Investment.InvestmentMappings
benchmarkMappings =
    either (error . Text.unpack) id $
        mkInvestmentMappings
            [ InvestmentMapping
                { assetAccount = "assets"
                , performanceAccounts =
                    [ PerformanceAccount "income:interest" Interest
                    , PerformanceAccount "income:dividend" Dividend
                    ]
                }
            ]

loadInvestmentInput :: Text.Text -> IO InvestmentInput
loadInvestmentInput fixturePath = do
    snapshot <- loadFixtureAt fixturePath
    let latest = maybe (error "investment benchmark fixture has no date") id snapshot.latestJournalDate
        monthEnds = monthlyPeriodEnds latest
    _ <- evaluate (rnf latest `seq` rnf monthEnds)
    pure InvestmentInput{snapshot, latest, monthEnds}

monthlyPeriodEnds :: Day -> [Day]
monthlyPeriodEnds latest =
    [ min latest (addDays (-1) (addGregorianMonthsClip 1 (addGregorianMonthsClip offset finalStart)))
    | offset <- [-23 .. 0]
    ]
  where
    (year, month, _) = toGregorian latest
    finalStart = fromGregorian year month 1

loadAndForceFixture :: IO Int
loadAndForceFixture = do
    result <- loadSnapshot "test/fixtures/shared.journal"
    case result of
        Left loadError -> fail (show loadError)
        Right snapshot -> evaluate (sum (length . Hledger.tpostings <$> snapshot.transactions))

loadTransactions :: IO [Hledger.Transaction]
loadTransactions = do
    (.transactions) <$> loadFixture

loadFixture :: IO Snapshot
loadFixture = loadFixtureAt "test/fixtures/shared.journal"

loadFixtureAt :: Text.Text -> IO Snapshot
loadFixtureAt fixturePath = do
    result <- loadSnapshot fixturePath
    case result of
        Left loadError -> fail (show loadError)
        Right snapshot -> do
            _ <- evaluate (sum (length . Hledger.tpostings <$> snapshot.transactions))
            pure snapshot

responseFor :: [Hledger.Transaction] -> TransactionsResponse
responseFor transactions =
    TransactionsResponse
        { metadata = ResponseMetadata{revision = "benchmark", freshness = "fresh", warnings = []}
        , transactions = toTransactions transactions
        , nextCursor = Nothing
        }

queryWeight :: Snapshot -> Text.Text -> Int
queryWeight snapshot query =
    case transactionPage snapshot TransactionPageRequest{query = Just query, limit = 50, cursor = Nothing} of
        Left _ -> 0
        Right page -> projectionWeight (toTransactions page.transactions)

projectionWeight :: [TransactionDto] -> Int
projectionWeight = sum . fmap transactionWeight

transactionWeight :: TransactionDto -> Int
transactionWeight transaction =
    length (show transaction.primaryDate)
        + maybe 0 (length . show) transaction.secondaryDate
        + Text.length transaction.status
        + Text.length transaction.description
        + maybe 0 Text.length transaction.code
        + maybe 0 Text.length transaction.comment
        + sum (tagWeight <$> transaction.tags)
        + sum (postingWeight <$> transaction.postings)
        + presentationWeight transaction.presentation

postingWeight :: PostingDto -> Int
postingWeight posting =
    Text.length posting.account
        + Text.length posting.status
        + maybe 0 Text.length posting.comment
        + sum (tagWeight <$> posting.tags)
        + sum (amountWeight <$> posting.amounts)

amountWeight :: AmountDto -> Int
amountWeight amount =
    Text.length amount.commodity
        + Text.length amount.quantity
        + maybe 0 costWeight amount.cost

costWeight :: CostDto -> Int
costWeight cost = Text.length cost.costType + Text.length cost.commodity + Text.length cost.quantity

tagWeight :: TagDto -> Int
tagWeight tag = Text.length tag.name + Text.length tag.value

presentationWeight :: TransactionPresentationDto -> Int
presentationWeight presentation =
    Text.length presentation.classification
