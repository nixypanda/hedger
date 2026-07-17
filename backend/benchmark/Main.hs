module Main (main) where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (evaluate)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text qualified as Text
import Hedger.Domain.Snapshot (Snapshot (transactions))
import Hedger.Infrastructure.Hledger.SnapshotStore (loadSnapshot)
import Hedger.Infrastructure.Hledger.TransactionQuery (TransactionPage (transactions), transactionPage)
import Hedger.Interface.HTTP.TransactionMapper (toTransactions)
import Hedger.Interface.HTTP.Types (
    AmountDto (..),
    CommodityQuantityDto (..),
    CostDto (..),
    PostingDto (..),
    ResponseMetadata (..),
    TagDto (..),
    TransactionDto (..),
    TransactionPresentationDto (..),
    TransactionsResponse (..),
 )
import Hledger.Data.Types qualified as Hledger
import Test.Tasty.Bench (Benchmark, bench, bgroup, defaultMain, env, nf, nfIO)

main :: IO ()
main =
    defaultMain
        [ bench "snapshot/parse-shared-13" $ nfIO loadAndForceFixture
        , queryBenchmarks "shared-13" "test/fixtures/shared.journal"
        , queryBenchmarks "large-50000" "../examples/performance/india-us-portfolio-large-50000.journal"
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

queryBenchmarks :: String -> Text.Text -> Benchmark
queryBenchmarks fixtureName fixturePath =
    env (BenchSnapshot <$> loadFixtureAt fixturePath) $ \(BenchSnapshot snapshot) ->
        bgroup
            ("query/" <> fixtureName)
            [ bench "account" $ nf (queryWeight snapshot) "acct:assets"
            , bench "boolean" $ nf (queryWeight snapshot) "expr:'(acct:expenses OR desc:Salary)'"
            ]

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
    case transactionPage snapshot (Just query) (Just "50") Nothing of
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
        + sum (Text.length <$> presentation.fromAccounts)
        + sum (Text.length <$> presentation.toAccounts)
        + sum (commodityQuantityWeight <$> presentation.totalAmounts)
        + maybe 0 id presentation.openingAccountCount

commodityQuantityWeight :: CommodityQuantityDto -> Int
commodityQuantityWeight amount = Text.length amount.commodity + Text.length amount.quantity
