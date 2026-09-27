module Main (
    main,
) where

import Control.DeepSeq (NFData (rnf))
import Control.Exception (evaluate)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Decimal (Decimal)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
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
import Hedger.Application.AppM (Env (..), runAppM)
import Hedger.Application.Common (
    AccountTypeClassifier,
    SnapshotMetadata (..),
    accountTypeClassifier,
 )
import Hedger.Application.Investments qualified as Investments
import Hedger.Application.PageRequest (allDates, defaultPageLimit)
import Hedger.Application.TransactionQuery (
    TransactionPage (transactions),
    TransactionPageRequest (..),
    transactionPage,
 )
import Hedger.Config (Config, mkConfig, mkPort)
import Hedger.Domain.Account (DisplayDepth (..))
import Hedger.Domain.Account qualified as Account
import Hedger.Domain.Activity (
    ActivityBuckets (..),
    ActivityFigures (..),
    ActivityKind (..),
    ActivityRange (..),
    AssetBucketFigures (..),
    ExpenseBucketFigures (..),
    IncomeBucketFigures (..),
    LiabilityBucketFigures (..),
    computeActivity,
 )
import Hedger.Domain.ActivityEvent (
    AssetMovement (..),
    FlowChange (..),
    LiabilityMovement (..),
    assetAccountName,
    expenseAccountName,
    incomeAccountName,
    liabilityAccountName,
    positiveAmountValue,
 )
import Hedger.Domain.Cashflow (CashflowFigures (..), computeCashflow)
import Hedger.Domain.Internal.Cashflow qualified as Cashflow
import Hedger.Domain.Internal.InvestmentPerformance qualified as Performance
import Hedger.Domain.Internal.NetWorth qualified as NetWorth
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
import Hedger.Domain.NetWorth (
    BreakdownItem (..),
    NetWorthFigures (..),
    PerformancePoint (..),
    TimelinePoint (..),
    computeNetWorth,
 )
import Hedger.Domain.Period (
    Granularity (..),
    MonthOfYear,
    fiscalYearPeriod,
    mkMonthOfYear,
    monthPeriod,
 )
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Domain.Valuation (Conversion, conversionMissing, conversionValue)
import Hedger.Domain.Xirr qualified as Xirr
import Hedger.Infrastructure.SnapshotStore (loadSnapshot, newSnapshotStore, reloadSnapshot)
import Hedger.Interface.HTTP.Core.Common (ResponseMetadata (..))
import Hedger.Interface.HTTP.Endpoints.Transactions (
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
import Hledger.Reports.MultiBalanceReport qualified as HledgerMultiBalance
import Hledger.Reports.ReportTypes qualified as HledgerReportTypes
import Test.Tasty (localOption)
import Test.Tasty.Bench (Benchmark, bench, bgroup, defaultMain, env, nf, nfIO)
import Test.Tasty.Runners (NumThreads (..))

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
        ( fmap
            (localOption (NumThreads 1))
            ( bgroup "parse" [bench name $ nfIO (loadAndForce path) | (name, path) <- fixtures]
                : fmap fixtureBenchmarks fixtures
            )
        )

fixtureBenchmarks :: (String, Text.Text) -> Benchmark
fixtureBenchmarks (name, path) =
    env (loadFixtureInput path) $ \input ->
        let snapshot = input.snapshot
            classify = accountTypeClassifier snapshot.journal
            context = prepareContext snapshot
         in bgroup
                name
                [ bgroup
                    "query"
                    [ bench "account" $ nf (queryWeight snapshot) "acct:assets"
                    , bench "boolean" $
                        nf (queryWeight snapshot) "expr:'(acct:expenses OR desc:Salary)'"
                    ]
                , bgroup
                    "investments"
                    [ bench "current" $ nf (summaryWeight . investmentSummary snapshot) input.latest
                    , bench "drill-down-singular-reference" $
                        nf
                            (sum . fmap summaryWeight . investmentDrillDownSingular snapshot)
                            input.latest
                    , bench "drill-down-latest" $
                        nf
                            (sum . fmap summaryWeight . investmentDrillDown snapshot)
                            input.latest
                    , bench "monthly-24" $
                        nf (sum . fmap summaryWeight . investmentSummaries snapshot) input.monthEnds
                    ]
                , bgroup
                    "application"
                    [ bench "investments" $
                        nfIO (investmentsReportWeight <$> investmentsReport input.env)
                    ]
                , bgroup
                    "investment-parts"
                    [ bench "prepare" $ nf prepareWeight snapshot
                    , bench "timeline-holdings-market" $
                        nf (forceAmountsList . timelineHoldingsMarket context) input.monthEnds
                    , bench "timeline-holdings-cost" $
                        nf (forceAmountsList . timelineHoldingsCost context) input.monthEnds
                    , bench "timeline-scope-scan" $
                        nf
                            (scopeScansWeight . Performance.scopeScansAt context Nothing)
                            input.monthEnds
                    , bench "drill-down-scope-scans" $
                        nf
                            ( scopeScansWeight
                                . Performance.scopeScansForScopesAt
                                    context
                                    (investmentDrillDownScopes snapshot input.latest)
                            )
                            input.latest
                    , bench "holdings-market" $
                        nf (forceAmounts . holdingsMarketAt snapshot) input.latest
                    , bench "holdings-cost" $
                        nf (forceAmounts . holdingsCostAt context) input.latest
                    , bench "accounts" $ nf (investmentAccountsCount snapshot) input.latest
                    , bench "mapping-complete" $
                        nf (fromEnum . mappingCompleteAt snapshot) input.latest
                    , bench "retained" $
                        nf (decimalWeight . retainedComponent context snapshot) input.latest
                    , bench "attributed" $
                        nf (attributionWeight . attributedComponent context snapshot) input.latest
                    , bench "cash-flows" $
                        nf (cashFlowsWeight . cashFlowsComponent context snapshot) input.latest
                    , bench "xirr-solve" $ nf xirrWeight input.investmentCashFlows
                    ]
                , bgroup
                    "analytics"
                    [ bench "networth" $ nf netWorthWeight snapshot
                    , bench "cashflow" $ nf cashflowWeight snapshot
                    , bench "asset-activity-month" $
                        nf
                            ( activityWeight
                                . computeActivity
                                    snapshot
                                    snapshot.transactions
                                    "INR"
                                    AssetActivity
                                    MonthActivity
                                    Depth2
                            )
                            (monthPeriod input.latest)
                    , bench "liability-activity-year" $
                        nf
                            ( activityWeight
                                . computeActivity
                                    snapshot
                                    snapshot.transactions
                                    "INR"
                                    LiabilityActivity
                                    FiscalYearActivity
                                    Depth2
                            )
                            (fiscalYearPeriod january input.latest)
                    ]
                , bgroup
                    "cashflow-parts"
                    [ bench "income-expense-periodic" $ nf incomeExpensePeriodicWeight snapshot
                    , bench "income-gain-periodic" $ nf incomeGainPeriodicWeight snapshot
                    , bench "expense-periodic" $ nf expensePeriodicWeight snapshot
                    ]
                , bgroup
                    "networth-parts"
                    [ bench "asset-liability-periodic" $ nf assetLiabilityPeriodicWeight snapshot
                    , bench "asset-cash-periodic" $ nf assetCashPeriodicWeight snapshot
                    , bench "liability-periodic" $ nf liabilityPeriodicWeight snapshot
                    , bench "gain-periodic" $ nf gainPeriodicWeight snapshot
                    , bench "current-asset-cash-balance" $ nf currentAssetCashWeight snapshot
                    , bench "current-liability-balance" $ nf currentLiabilityWeight snapshot
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
    , env :: Env
    , latest :: Day
    , monthEnds :: [Day]
    , xirrFlows :: [Xirr.CashFlow]
    , investmentCashFlows :: [Xirr.CashFlow]
    , page :: [Hledger.Transaction]
    }

instance NFData FixtureInput where
    rnf input =
        rnf (forceSnapshot input.snapshot) `seq`
            rnf input.latest `seq`
                rnf input.monthEnds `seq`
                    rnf (length (show input.xirrFlows)) `seq`
                        rnf (length (show input.investmentCashFlows)) `seq`
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
    store <- newSnapshotStore path
    loaded <- reloadSnapshot store
    snapshot <-
        case loaded of
            Left loadError -> fail (show loadError)
            Right loadedSnapshot -> do
                _ <- evaluate (forceSnapshot loadedSnapshot)
                pure loadedSnapshot
    let configuration = benchmarkConfig path
        environment = Env{config = configuration, snapshotStore = store}
        latest = fromMaybe (error "benchmark fixture has no date") snapshot.latestJournalDate
        transactions =
            Performance.investmentDataTransactions snapshot benchmarkAssetClassMappings Nothing
        monthEnds = (\(_, _, end) -> end) <$> Performance.reportPeriods Monthly transactions latest
        current = investmentSummary snapshot latest
        context = prepareContext snapshot
        terminal = Xirr.cashFlow latest (fromMaybe 0 (Performance.summaryMarketValue current))
        investmentCashFlows =
            fromMaybe [] (cashFlowsComponent context snapshot latest) <> [terminal]
    _ <- evaluate (length (show configuration))
    _ <- evaluate environment
    case Performance.summaryMarketValue current of
        Nothing -> fail "investment benchmark fixture produced an empty portfolio"
        Just _ -> pure ()
    pure
        FixtureInput
            { snapshot
            , env = environment
            , latest
            , monthEnds
            , xirrFlows = deriveXirrFlows snapshot
            , investmentCashFlows
            , -- A production transactions page is capped at 'maximumPageLimit' (200),
              -- so projection/encoding never see more than that many rows.
              page = take 200 snapshot.transactions
            }

benchmarkConfig :: Text.Text -> Config
benchmarkConfig path =
    either (error . Text.unpack) id $
        mkConfig
            path
            "127.0.0.1"
            (either (error . Text.unpack) id (mkPort 8080))
            "INR"
            (maybe (error "invalid fiscal month") id (mkMonthOfYear 4))
            benchmarkMappings
            benchmarkAssetClassMappings

january :: MonthOfYear
january = maybe (error "invalid fiscal month") id (mkMonthOfYear 1)

loadAndForce :: Text.Text -> IO Int
loadAndForce path = do
    result <- loadSnapshot path
    case result of
        Left loadError -> fail (show loadError)
        Right snapshot -> evaluate (forceSnapshot snapshot)

-- * Query
queryWeight :: Snapshot -> Text.Text -> Int
queryWeight snapshot query =
    case transactionPage
        snapshot
        TransactionPageRequest
            { query = Just query
            , limit = defaultPageLimit
            , cursor = Nothing
            } of
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
     in Performance.summariesAtPrepared context Nothing Monthly days

investmentDrillDown :: Snapshot -> Day -> [Performance.InvestmentSummary]
investmentDrillDown snapshot day =
    snd
        <$> Performance.summariesAtScopesPrepared
            context
            day
            (investmentDrillDownScopes snapshot day)
  where
    context = prepareContext snapshot

investmentDrillDownSingular :: Snapshot -> Day -> [Performance.InvestmentSummary]
investmentDrillDownSingular snapshot day =
    fmap
        (\scope -> Performance.summaryAtPrepared context scope day)
        (investmentDrillDownScopes snapshot day)
  where
    context = prepareContext snapshot

investmentDrillDownScopes :: Snapshot -> Day -> [Maybe Text.Text]
investmentDrillDownScopes snapshot day = List.nub (Nothing : fmap Just children <> allocationScopes)
  where
    accounts = Performance.investmentAccountsAt snapshot benchmarkAssetClassMappings Nothing day
    children = Performance.childrenOf Nothing accounts
    allocationScopes =
        [ Just (Investment.assetAccountText prefix)
        | mapping <- Investment.assetClassMappingsList benchmarkAssetClassMappings
        , prefix <- NonEmpty.toList mapping.accountPrefixes
        ]

prepareContext :: Snapshot -> Performance.InvestmentPerformanceContext
prepareContext snapshot =
    Performance.prepareInvestmentPerformance
        snapshot
        benchmarkAssetClassMappings
        benchmarkMappings
        "INR"

prepareWeight :: Snapshot -> Int
prepareWeight snapshot =
    forcePostingContents (Hledger.jtxns (prepareContext snapshot).historicalCostJournal)

mappingCompleteAt :: Snapshot -> Day -> Bool
mappingCompleteAt snapshot day =
    not (null accounts)
        && all (Performance.hasAttributablePerformance snapshot benchmarkMappings day) accounts
  where
    accounts = Performance.investmentAccountsAt snapshot benchmarkAssetClassMappings Nothing day

holdingsMarketAt :: Snapshot -> Day -> Hledger.MixedAmount
holdingsMarketAt snapshot = Performance.holdingsAt snapshot benchmarkAssetClassMappings Nothing

holdingsCostAt :: Performance.InvestmentPerformanceContext -> Day -> Hledger.MixedAmount
holdingsCostAt context =
    Performance.holdingsAtWithJournal
        context.historicalCostJournal
        benchmarkAssetClassMappings
        Nothing

investmentAccountsCount :: Snapshot -> Day -> Int
investmentAccountsCount snapshot day =
    length (Performance.investmentAccountsAt snapshot benchmarkAssetClassMappings Nothing day)

retainedComponent :: Performance.InvestmentPerformanceContext -> Snapshot -> Day -> Maybe Decimal
retainedComponent context snapshot =
    Performance.retainedPerformanceAtWith
        context.valueAt
        snapshot
        benchmarkAssetClassMappings
        benchmarkMappings
        "INR"
        Nothing

attributedComponent ::
    Performance.InvestmentPerformanceContext ->
    Snapshot ->
    Day ->
    Maybe Performance.Attribution
attributedComponent context snapshot =
    Performance.attributedAtWith
        context.valueAt
        snapshot
        benchmarkAssetClassMappings
        benchmarkMappings
        "INR"
        Nothing

cashFlowsComponent ::
    Performance.InvestmentPerformanceContext -> Snapshot -> Day -> Maybe [Xirr.CashFlow]
cashFlowsComponent context snapshot =
    Performance.externalCashFlowsAtWith
        context.valueAt
        snapshot
        benchmarkAssetClassMappings
        benchmarkMappings
        "INR"
        Nothing

decimalWeight :: Maybe Decimal -> Int
decimalWeight = length . show

attributionWeight :: Maybe Performance.Attribution -> Int
attributionWeight = length . show

cashFlowsWeight :: Maybe [Xirr.CashFlow] -> Int
cashFlowsWeight = length . show

forceAmountsList :: [Hledger.MixedAmount] -> Int
forceAmountsList = sum . fmap forceAmounts

scopeScansWeight :: [Performance.ScopeScan] -> Int
scopeScansWeight = sum . fmap scopeScanWeight

scopeScanWeight :: Performance.ScopeScan -> Int
scopeScanWeight scan =
    sum (Text.length <$> scan.accounts)
        + fromEnum scan.mappingComplete
        + decimalWeight scan.retained
        + attributionWeight scan.attributed
        + cashFlowsWeight scan.cashFlows

timelineHoldingsMarket :: Performance.InvestmentPerformanceContext -> [Day] -> [Hledger.MixedAmount]
timelineHoldingsMarket context =
    Performance.holdingsSeriesAtWithJournal
        context.snapshot.journal
        context.assetClasses
        Nothing
        Monthly

timelineHoldingsCost :: Performance.InvestmentPerformanceContext -> [Day] -> [Hledger.MixedAmount]
timelineHoldingsCost context =
    Performance.holdingsSeriesAtWithJournal
        context.historicalCostJournal
        context.assetClasses
        Nothing
        Monthly

summaryWeight :: Performance.InvestmentSummary -> Int
summaryWeight summary =
    sum
        [ length (show (Performance.summaryMarketValue summary))
        , length (show (Performance.summaryPurchaseCost summary))
        , length (show (Performance.summaryGainLoss summary))
        , length (show (Performance.summaryXirr summary))
        , length (show (Performance.summaryAttributedIncome summary))
        , length (show (Performance.summaryRealisedGain summary))
        , length (show (Performance.summaryTotalProfit summary))
        , length (show (Performance.summaryCompleteness summary))
        , length (show (Performance.summaryWarnings summary))
        ]

{- | Run the complete in-memory application use case. Fixture parsing and the
snapshot store's initial publication happen in 'loadFixtureInput'; this timed
action starts with the same populated store an HTTP request would read.
-}
investmentsReport :: Env -> IO Investments.InvestmentsReport
investmentsReport environment = do
    result <-
        runAppM
            environment
            ( Investments.investments
                Investments.InvestmentsRequest
                    { Investments.granularity = Monthly
                    , Investments.displayCommodity = Nothing
                    , Investments.accountPrefix = Nothing
                    }
            )
    case result of
        Left appError -> fail (show appError)
        Right report -> pure report

{- | Force every response field that would be materialised by the HTTP layer.
This keeps the benchmark focused on the application's full report calculation,
including timeline, latest summary, child and allocation projections.

Keep this field-by-field rather than relying on an 'NFData' instance: changes to
the application read model then require an explicit benchmark audit here.
-}
investmentsReportWeight :: Investments.InvestmentsReport -> Int
investmentsReportWeight report =
    metadataWeight report.metadata
        + Text.length report.displayCommodity
        + length (show report.granularity)
        + maybe 0 Text.length report.accountPrefix
        + summaryWeight report.summary
        + sum (investmentPointWeight <$> report.timeline)
        + sum (breadcrumbWeight <$> report.breadcrumbs)
        + sum (investmentChildWeight <$> report.children)
        + sum (assetClassAllocationWeight <$> report.assetClassAllocations)
        + length (show report.warnings)

metadataWeight :: SnapshotMetadata -> Int
metadataWeight responseMetadata =
    Text.length responseMetadata.revision
        + length (show responseMetadata.freshness)

investmentPointWeight :: Investments.InvestmentPoint -> Int
investmentPointWeight point =
    Text.length point.period + length (show point.startDate) + summaryWeight point.summary

breadcrumbWeight :: Investments.InvestmentBreadcrumb -> Int
breadcrumbWeight breadcrumb = Text.length breadcrumb.account + Text.length breadcrumb.label

investmentChildWeight :: Investments.InvestmentChild -> Int
investmentChildWeight child =
    Text.length child.account
        + Text.length child.label
        + summaryWeight child.summary
        + fromEnum child.hasChildren
        + length (show child.assetClass)
        + length (show child.allocationPercentage)

assetClassAllocationWeight :: Investments.AssetClassAllocation -> Int
assetClassAllocationWeight allocation =
    length (show allocation.assetClass)
        + length (show allocation.marketValue)
        + length (show allocation.currentPercentage)
        + length (show allocation.targetPercentage)

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
            { assetAccount = mkAssetAccountOrError assetAccount
            , performanceAccounts =
                [ PerformanceAccount (mkAccountNameOrError account) OtherIncome
                | account <- performanceAccounts
                ]
            }

mkAssetAccountOrError :: Text.Text -> Investment.AssetAccount
mkAssetAccountOrError = either (error . Text.unpack) id . Investment.mkAssetAccount

mkAccountNameOrError :: Text.Text -> Investment.AccountName
mkAccountNameOrError = either (error . Text.unpack) id . Investment.mkAccountName

benchmarkAssetClassMappings :: Investment.AssetClassMappings
benchmarkAssetClassMappings =
    either (error . Text.unpack) id $
        mkAssetClassMappings
            [ AssetClassMapping Equity (assetPrefix "assets:equity") (Just (percentage 60))
            , AssetClassMapping Debt (assetPrefix "assets:debt") (Just (percentage 25))
            , AssetClassMapping Cash (assetPrefix "assets:liquid") (Just (percentage 5))
            , AssetClassMapping Hybrid (assetPrefix "assets:hybrid") (Just (percentage 10))
            ]
  where
    assetPrefix name = mkAssetAccountOrError name :| []
    percentage value = either (error . Text.unpack) id (Investment.mkPercentage value)

-- * Analytics: net worth and cashflow both iterate every period across the

-- whole journal, so their cost scales with transaction count.

{- | Every field is forced: the strict current balances are lazy 'Maybe's that
a partial weight would silently skip, hiding two whole report passes.
-}
netWorthWeight :: Snapshot -> Int
netWorthWeight snapshot =
    let figures =
            computeNetWorth
                snapshot
                snapshot.transactions
                Monthly
                "INR"
                Nothing
                Nothing
                Depth2
                Depth2
     in sum (timelineWeight <$> figures.timeline)
            + sum (breakdownWeight <$> figures.assetsBreakdownItems)
            + sum (breakdownWeight <$> figures.liabilitiesBreakdownItems)
            + sum (performanceWeight <$> figures.performance)
            + figures.incompletePeriods
            + maybe 0 Text.length figures.selectedKey
            + decimalWeight figures.currentAssets
            + decimalWeight figures.currentLiabilities
            + decimalWeight figures.currentNetWorth
            + Text.length figures.assetsBreakdownPeriod
            + Text.length figures.liabilitiesBreakdownPeriod
            + Set.size figures.missing

cashflowWeight :: Snapshot -> Int
cashflowWeight snapshot =
    let figures =
            computeCashflow
                snapshot
                snapshot.transactions
                Monthly
                "INR"
                Nothing
                Nothing
                Depth2
                Depth2
     in sum (timelineWeight <$> figures.timeline)
            + sum (breakdownWeight <$> figures.incomeBreakdownItems)
            + sum (breakdownWeight <$> figures.expenseBreakdownItems)
            + figures.totalCount
            + figures.filteredCount
            + fromEnum figures.available
            + Text.length figures.incomeBreakdownPeriod
            + Text.length figures.expenseBreakdownPeriod
            + Set.size figures.missing

activityWeight :: ActivityFigures -> Int
activityWeight figures =
    Set.size figures.missing
        + Set.size figures.issues
        + case figures.buckets of
            AssetBuckets buckets -> sum (fmap assetBucketWeight buckets)
            LiabilityBuckets buckets -> sum (fmap liabilityBucketWeight buckets)
            IncomeBuckets buckets -> sum (fmap incomeBucketWeight buckets)
            ExpenseBuckets buckets -> sum (fmap expenseBucketWeight buckets)

assetBucketWeight :: AssetBucketFigures -> Int
assetBucketWeight bucket =
    Text.length bucket.key
        + decimalWeight (Just bucket.acquired)
        + decimalWeight (Just bucket.disposed)
        + decimalWeight (Just bucket.transferred)
        + sum (fmap breakdownWeight bucket.categoryItems)
        + sum (fmap assetMovementWeight bucket.movements)
        + length bucket.transactions

liabilityBucketWeight :: LiabilityBucketFigures -> Int
liabilityBucketWeight bucket =
    Text.length bucket.key
        + decimalWeight (Just bucket.borrowed)
        + decimalWeight (Just bucket.repaid)
        + decimalWeight (Just bucket.forgiven)
        + decimalWeight (Just bucket.borrowingReversed)
        + sum (fmap breakdownWeight bucket.categoryItems)
        + sum (fmap liabilityMovementWeight bucket.movements)
        + length bucket.transactions

incomeBucketWeight :: IncomeBucketFigures -> Int
incomeBucketWeight bucket =
    Text.length bucket.key
        + decimalWeight (Just bucket.posted)
        + decimalWeight (Just bucket.reversed)
        + sum (fmap breakdownWeight bucket.categoryItems)
        + sum
            [ Text.length (incomeAccountName account) + flowWeight change
            | (account, change) <- bucket.changes
            ]
        + length bucket.transactions

expenseBucketWeight :: ExpenseBucketFigures -> Int
expenseBucketWeight bucket =
    Text.length bucket.key
        + decimalWeight (Just bucket.posted)
        + decimalWeight (Just bucket.reversed)
        + sum (fmap breakdownWeight bucket.categoryItems)
        + sum
            [ Text.length (expenseAccountName account) + flowWeight change
            | (account, change) <- bucket.changes
            ]
        + length bucket.transactions

assetMovementWeight :: AssetMovement -> Int
assetMovementWeight movement =
    case movement of
        Acquired account amount ->
            Text.length (assetAccountName account)
                + decimalWeight (Just (positiveAmountValue amount))
        Disposed account amount ->
            Text.length (assetAccountName account)
                + decimalWeight (Just (positiveAmountValue amount))
        Transferred source outgoing destination incoming ->
            Text.length (assetAccountName source)
                + Text.length (assetAccountName destination)
                + decimalWeight (Just (positiveAmountValue outgoing))
                + decimalWeight (Just (positiveAmountValue incoming))

liabilityMovementWeight :: LiabilityMovement -> Int
liabilityMovementWeight movement =
    case movement of
        Borrowed account amount -> single account amount
        Repaid account amount -> single account amount
        Forgiven account amount -> single account amount
        BorrowingReversed account amount -> single account amount
        Refinanced old repaid new borrowed -> single old repaid + single new borrowed
  where
    single account amount =
        Text.length (liabilityAccountName account)
            + decimalWeight (Just (positiveAmountValue amount))

flowWeight :: FlowChange -> Int
flowWeight change =
    case change of
        Posted amount -> decimalWeight (Just (positiveAmountValue amount))
        Reversed amount -> decimalWeight (Just (positiveAmountValue amount))

timelineWeight :: (Show side) => TimelinePoint side -> Int
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

incomeExpensePeriodicWeight :: Snapshot -> Int
incomeExpensePeriodicWeight snapshot =
    periodicReportWeight
        (Cashflow.incomeExpensePeriodicReport snapshot snapshot.transactions Monthly "INR")

incomeGainPeriodicWeight :: Snapshot -> Int
incomeGainPeriodicWeight snapshot =
    periodicReportWeight
        (Cashflow.incomeGainPeriodicReport snapshot snapshot.transactions Monthly "INR" Depth2)

expensePeriodicWeight :: Snapshot -> Int
expensePeriodicWeight snapshot =
    periodicReportWeight
        (Cashflow.expensePeriodicReport snapshot snapshot.transactions Monthly "INR" Depth2)

assetCashPeriodicWeight :: Snapshot -> Int
assetCashPeriodicWeight snapshot =
    periodicReportWeight
        (NetWorth.assetCashPeriodicReport snapshot snapshot.transactions Monthly "INR" Depth2)

liabilityPeriodicWeight :: Snapshot -> Int
liabilityPeriodicWeight snapshot =
    periodicReportWeight
        (NetWorth.liabilityPeriodicReport snapshot snapshot.transactions Monthly "INR" Depth2)

assetLiabilityPeriodicWeight :: Snapshot -> Int
assetLiabilityPeriodicWeight snapshot =
    periodicReportWeight
        (NetWorth.assetLiabilityPeriodicReport snapshot snapshot.transactions Monthly "INR")

gainPeriodicWeight :: Snapshot -> Int
gainPeriodicWeight snapshot =
    periodicReportWeight (NetWorth.gainPeriodicReport snapshot snapshot.transactions Monthly "INR")

currentAssetCashWeight :: Snapshot -> Int
currentAssetCashWeight snapshot =
    conversionWeight (NetWorth.currentAssetCashBalance snapshot snapshot.transactions "INR")

currentLiabilityWeight :: Snapshot -> Int
currentLiabilityWeight snapshot =
    conversionWeight (NetWorth.currentLiabilityBalance snapshot snapshot.transactions "INR")

periodicReportWeight :: HledgerMultiBalance.MultiBalanceReport -> Int
periodicReportWeight report =
    length (show report.prDates)
        + sum (forceAmounts <$> report.prTotals.prrAmounts)
        + sum
            [ Text.length row.prrName.displayFull + sum (forceAmounts <$> row.prrAmounts)
            | row <- report.prRows
            ]

conversionWeight :: Conversion Decimal -> Int
conversionWeight conversion =
    maybe 0 (length . show) (conversionValue conversion) + Set.size (conversionMissing conversion)

-- * Accounts: the tree values every account against the price oracle; the

-- register builds an account-transactions report over every matching posting.
accountTreeWeight :: Snapshot -> Int
accountTreeWeight snapshot = sum (nodeWeight context <$> snapshot.accountTree.asubs)
  where
    valuationDate = fromMaybe (fromGregorian 1970 1 1) snapshot.latestJournalDate
    context = Account.mkValuationContext snapshot.journal "INR" valuationDate

nodeWeight :: Account.ValuationContext -> Hledger.Account Hledger.BalanceData -> Int
nodeWeight context account =
    let total = foldMap id (account.adata.pdpre : Map.elems account.adata.pdperiods)
        valued =
            Account.valueBalances
                context
                (Hledger.bdexcludingsubs total)
                (Hledger.bdincludingsubs total)
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
            , dateRange = allDates
            , limit = defaultPageLimit
            , cursor = Nothing
            }

entryWeight :: RegisterEntry -> Int
entryWeight entry =
    sum (Text.length <$> entry.matchedAccounts) + length (show entry.displayItem)

-- * Xirr
xirrWeight :: [Xirr.CashFlow] -> Int
xirrWeight = length . show . Xirr.xirr

deriveXirrFlows :: Snapshot -> [Xirr.CashFlow]
deriveXirrFlows snapshot =
    case (.tdate) <$> snapshot.transactions of
        [] ->
            [ Xirr.cashFlow (fromGregorian 2024 1 1) (-1000)
            , Xirr.cashFlow (fromGregorian 2025 1 1) 1100
            ]
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
    Text.length amount.commodity + Text.length amount.quantity + maybe 0 costWeight amount.cost

costWeight :: CostDto -> Int
costWeight cost = Text.length cost.costType + Text.length cost.commodity + Text.length cost.quantity

tagWeight :: TagDto -> Int
tagWeight tag = Text.length tag.name + Text.length tag.value

presentationWeight :: TransactionPresentationDto -> Int
presentationWeight presentation = Text.length presentation.classification
