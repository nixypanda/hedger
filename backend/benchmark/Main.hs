module Main (main) where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (evaluate)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time (Day, addDays, fromGregorian)
import Hedger.Application.AccountRegisterQuery (
    RegisterEntry (..),
    RegisterPage (..),
    RegisterPageRequest (..),
    registerPage,
 )
import Hedger.Application.Common (AccountTypeClassifier, accountTypeClassifier)
import Hedger.Application.TransactionQuery (
    TransactionPage (transactions),
    TransactionPageRequest (..),
    transactionPage,
 )
import Hedger.Domain.Account qualified as Account
import Hedger.Domain.Cashflow (CashflowFigures (..), computeCashflow)
import Hedger.Domain.Investment (
    AssetClass (..),
    AssetClassMapping (..),
    InvestmentMapping (..),
    PerformanceAccount (..),
    PerformanceRole (..),
    mkAssetClassMappings,
    mkInvestmentMappings,
 )
import Hedger.Domain.Investment qualified as Investment
import Hedger.Domain.InvestmentPerformance qualified as Performance
import Hedger.Domain.NetWorth (
    BreakdownItem (..),
    NetWorthFigures (..),
    PerformancePoint (..),
    TimelinePoint (..),
    computeNetWorth,
 )
import Hedger.Domain.Period (Granularity (..))
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
import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Types qualified as Hledger
import Test.Tasty.Bench (Benchmark, bench, bgroup, defaultMain, env, nf, nfIO)

{- | Only sizes with enough transactions to exercise scaling behaviour. Each
fixture is loaded once and shared across every benchmark group below.
-}
fixtures :: [(String, Text.Text)]
fixtures =
    [ ("medium-5000", "../examples/performance/india-us-portfolio-medium-5000.journal")
    , ("large-50000", "../examples/performance/india-us-portfolio-large-50000.journal")
    ]

main :: IO ()
main =
    defaultMain
        ( bgroup "parse" [bench name $ nfIO (loadAndForce path) | (name, path) <- fixtures]
            : fmap fixtureBenchmarks fixtures
        )

fixtureBenchmarks :: (String, Text.Text) -> Benchmark
fixtureBenchmarks (name, path) =
    env (loadFixtureInput path) $ \input ->
        let snapshot = input.snapshot
            classify = accountTypeClassifier snapshot.journal
         in bgroup
                name
                [ bgroup
                    "query"
                    [ bench "account" $ nf (queryWeight snapshot) "acct:assets"
                    , bench "boolean" $ nf (queryWeight snapshot) "expr:'(acct:expenses OR desc:Salary)'"
                    ]
                , bgroup
                    "investments"
                    [ bench "current" $ nf (summaryWeight . investmentSummary snapshot) input.latest
                    , bench "monthly-24" $
                        nf (sum . fmap summaryWeight . investmentSummaries snapshot) input.monthEnds
                    ]
                , bgroup
                    "analytics"
                    [ bench "networth" $ nf netWorthWeight snapshot
                    , bench "cashflow" $ nf cashflowWeight snapshot
                    ]
                , bgroup
                    "accounts"
                    [ bench "tree" $ nf accountTreeWeight snapshot
                    , bench "register" $ nf (registerWeight snapshot) "assets"
                    ]
                , bgroup "xirr" [bench "journal-derived" $ nf xirrWeight input.xirrFlows]
                , bgroup
                    "transactions"
                    [ bench "project" $ nf (projectionWeight . toTransactions classify) input.page
                    , bench "encode" $
                        nf (LazyByteString.length . Aeson.encode . responseFor classify) input.page
                    ]
                ]

{- | The inputs a fixture's benchmarks need, precomputed so 'env' forces them
(and the parse cost) out of the timed region. A classifier is derived per
use inside 'fixtureBenchmarks' since functions have no 'NFData'.
-}
data FixtureInput = FixtureInput
    { snapshot :: Snapshot
    , latest :: Day
    , monthEnds :: [Day]
    , xirrFlows :: [Xirr.CashFlow]
    , page :: [Hledger.Transaction]
    }

instance NFData FixtureInput where
    rnf input =
        rnf (forceSnapshot input.snapshot) `seq`
            rnf input.latest `seq`
                rnf input.monthEnds `seq`
                    rnf (length (show input.xirrFlows)) `seq`
                        rnf (forcePostingContents input.page)

{- | Force everything a fixture's benchmarks read out of a snapshot, so 'env'
pays derivation cost up front rather than inside a timed benchmark. hledger
parses strictly, so in practice the one field this materialises is the
lazily built 'accountTree'; forcing it keeps the parse benchmark honest. Since
hledger-lib has no 'NFData', we force by summing numeric leaves rather than via
'show', which would drag in amount-display machinery no benchmark exercises.
-}
forceSnapshot :: Snapshot -> Int
forceSnapshot snapshot =
    forcePostingContents snapshot.transactions
        + forceAccountTree snapshot.accountTree
        + maybe 0 (const 1) snapshot.latestJournalDate

forcePostingContents :: [Hledger.Transaction] -> Int
forcePostingContents txns =
    sum
        [ Text.length (Hledger.paccount posting) + forceAmounts (Hledger.pamount posting)
        | txn <- txns
        , posting <- Hledger.tpostings txn
        ]

forceAccountTree :: Hledger.Account Hledger.BalanceData -> Int
forceAccountTree account =
    Text.length account.aname
        + forceAmounts (Hledger.bdincludingsubs account.adata.pdpre)
        + sum (forceAmounts . Hledger.bdincludingsubs <$> account.adata.pdperiods)
        + sum (forceAccountTree <$> account.asubs)

forceAmounts :: Hledger.MixedAmount -> Int
forceAmounts mixed =
    sum [fromInteger (truncate (Hledger.aquantity amount)) | amount <- HledgerAmount.amounts mixed]

loadFixtureInput :: Text.Text -> IO FixtureInput
loadFixtureInput path = do
    snapshot <- loadFixtureAt path
    let latest = fromMaybe (error "benchmark fixture has no date") snapshot.latestJournalDate
        transactions = Performance.investmentDataTransactions snapshot benchmarkAssetClassMappings Nothing
        monthEnds = (\(_, _, end) -> end) <$> Performance.reportPeriods Monthly transactions latest
        current = investmentSummary snapshot latest
    case current.marketValue of
        Nothing -> fail "investment benchmark fixture produced an empty portfolio"
        Just _ -> pure ()
    pure
        FixtureInput
            { snapshot
            , latest
            , monthEnds
            , xirrFlows = deriveXirrFlows snapshot
            , -- A production transactions page is capped at 'maximumPageLimit' (200),
              -- so projection/encoding never see more than that many rows.
              page = take 200 snapshot.transactions
            }

loadAndForce :: Text.Text -> IO Int
loadAndForce path = do
    result <- loadSnapshot path
    case result of
        Left loadError -> fail (show loadError)
        Right snapshot -> evaluate (forceSnapshot snapshot)

loadFixtureAt :: Text.Text -> IO Snapshot
loadFixtureAt path = do
    result <- loadSnapshot path
    case result of
        Left loadError -> fail (show loadError)
        Right snapshot -> do
            _ <- evaluate (forceSnapshot snapshot)
            pure snapshot

-- * Query

queryWeight :: Snapshot -> Text.Text -> Int
queryWeight snapshot query =
    case transactionPage snapshot TransactionPageRequest{query = Just query, limit = 50, cursor = Nothing} of
        Left _ -> 0
        Right page ->
            projectionWeight
                (toTransactions (accountTypeClassifier snapshot.journal) page.transactions)

-- * Investments

investmentSummary :: Snapshot -> Day -> Performance.InvestmentSummary
investmentSummary snapshot day =
    Performance.investmentSummaryAt
        snapshot
        benchmarkAssetClassMappings
        benchmarkMappings
        "INR"
        Nothing
        day

investmentSummaries :: Snapshot -> [Day] -> [Performance.InvestmentSummary]
investmentSummaries snapshot days =
    let context =
            Performance.prepareInvestmentPerformance
                snapshot
                benchmarkAssetClassMappings
                benchmarkMappings
                "INR"
     in [ (Performance.summaryAtPrepared context Nothing day).summary
        | day <- days
        ]

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
            [ mapping "assets:debt:fd" ["income:interest:fd"]
            , mapping "assets:debt:epf:india:nimbusworks" ["income:interest:epf"]
            , mapping "assets:debt:eps:india:nimbusworks" ["income:interest:eps"]
            , mapping "assets:debt:ppf:india:varuna" ["income:interest:ppf"]
            , mapping "assets:hybrid:india:nps:monsoon" ["income:interest:nps"]
            , mapping "assets:liquid" ["income:interest:liquid"]
            , mapping "assets:equity" ["income:dividend:india", "income:dividend:us"]
            ]
  where
    mapping assetAccount performanceAccounts =
        InvestmentMapping
            { assetAccount
            , performanceAccounts =
                [ PerformanceAccount account OtherIncome
                | account <- performanceAccounts
                ]
            }

benchmarkAssetClassMappings :: Investment.AssetClassMappings
benchmarkAssetClassMappings =
    either (error . Text.unpack) id $
        mkAssetClassMappings
            [ AssetClassMapping Equity ["assets:equity"] (Just 60)
            , AssetClassMapping Debt ["assets:debt"] (Just 25)
            , AssetClassMapping Cash ["assets:liquid"] (Just 5)
            , AssetClassMapping Hybrid ["assets:hybrid"] (Just 10)
            ]

-- * Analytics: net worth and cashflow both iterate every period across the

-- whole journal, so their cost scales with transaction count.

netWorthWeight :: Snapshot -> Int
netWorthWeight snapshot =
    let figures = computeNetWorth snapshot snapshot.transactions Monthly "INR" Nothing Nothing 2 2
     in sum (timelineWeight <$> figures.timeline)
            + sum (breakdownWeight <$> figures.assetsBreakdownItems)
            + sum (breakdownWeight <$> figures.liabilitiesBreakdownItems)
            + sum (performanceWeight <$> figures.performance)
            + figures.incompletePeriods

cashflowWeight :: Snapshot -> Int
cashflowWeight snapshot =
    let figures = computeCashflow snapshot snapshot.transactions Monthly "INR" Nothing Nothing 2 2
     in sum (timelineWeight <$> figures.timeline)
            + sum (breakdownWeight <$> figures.incomeBreakdownItems)
            + sum (breakdownWeight <$> figures.expenseBreakdownItems)
            + figures.totalCount
            + figures.filteredCount

timelineWeight :: TimelinePoint -> Int
timelineWeight point =
    length (show point.positive)
        + length (show point.negative)
        + length (show point.net)
        + Set.size point.missing

breakdownWeight :: BreakdownItem -> Int
breakdownWeight item = Text.length item.account + length (show item.quantity)

performanceWeight :: PerformancePoint -> Int
performanceWeight point =
    length (show point.netInvestment) + length (show point.netWorth) + length (show point.gainLoss)

-- * Accounts: the tree values every account against the price oracle; the

-- register builds an account-transactions report over every matching posting.

accountTreeWeight :: Snapshot -> Int
accountTreeWeight snapshot =
    sum (nodeWeight context <$> snapshot.accountTree.asubs)
  where
    valuationDate = fromMaybe (fromGregorian 1970 1 1) snapshot.latestJournalDate
    context = Account.mkValuationContext snapshot.journal "INR" valuationDate

nodeWeight :: Account.ValuationContext -> Hledger.Account Hledger.BalanceData -> Int
nodeWeight context account =
    let total = foldMap id (account.adata.pdpre : Map.elems account.adata.pdperiods)
        valued = Account.valueBalances context (Hledger.bdexcludingsubs total) (Hledger.bdincludingsubs total)
     in length (show valued.directBalance)
            + length (show valued.subtreeBalance)
            + sum (nodeWeight context <$> account.asubs)

registerWeight :: Snapshot -> Text.Text -> Int
registerWeight snapshot account =
    case registerPage snapshot "INR" request of
        Left _ -> 0
        Right page -> sum (entryWeight <$> page.entries)
  where
    request =
        RegisterPageRequest
            { account
            , commodity = Just "INR"
            , start = Nothing
            , end = Nothing
            , limit = 50
            , cursor = Nothing
            }

entryWeight :: RegisterEntry -> Int
entryWeight entry = sum (Text.length <$> entry.matchedAccounts) + length (show entry.displayItem)

-- * Xirr

xirrWeight :: [Xirr.CashFlow] -> Int
xirrWeight = length . show . Xirr.xirr

deriveXirrFlows :: Snapshot -> [Xirr.CashFlow]
deriveXirrFlows snapshot =
    case (.tdate) <$> snapshot.transactions of
        [] -> [Xirr.cashFlow (fromGregorian 2024 1 1) (-1000), Xirr.cashFlow (fromGregorian 2025 1 1) 1100]
        dates@(first : rest) ->
            let contributionCount = length rest + 1
                terminal = addDays 365 (maximum dates)
             in Xirr.cashFlow first (-1)
                    : [Xirr.cashFlow day (-1) | day <- rest]
                        <> [Xirr.cashFlow terminal (fromIntegral (contributionCount + 1000))]

-- * Transactions projection and JSON encoding

responseFor :: AccountTypeClassifier -> [Hledger.Transaction] -> TransactionsResponse
responseFor classify transactions =
    TransactionsResponse
        { metadata = ResponseMetadata{revision = "benchmark", freshness = "fresh", warnings = []}
        , transactions = toTransactions classify transactions
        , nextCursor = Nothing
        }

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
