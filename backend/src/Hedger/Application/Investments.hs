module Hedger.Application.Investments (
    InvestmentsRequest (..),
    InvestmentPoint (..),
    InvestmentBreadcrumb (..),
    InvestmentChild (..),
    AssetClassAllocation (..),
    InvestmentsReport (..),
    investments,
)
where

import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Decimal (Decimal)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)

import Hledger.Data.AccountName qualified as HledgerAccountName

import Hedger.Application.AppM (AppM, Env (..), SnapshotContext (..), requireSnapshot)
import Hedger.Application.Cache (contributeCheckpoints, usableCheckpoints)
import Hedger.Application.Common (SnapshotMetadata, currentMetadata)
import Hedger.Config (Config (..))
import Hedger.Domain.Error (AppError (InternalError))
import Hedger.Domain.Internal.InvestmentCheckpoint (checkpointCount, solvedRateCount)
import Hedger.Domain.Investment qualified as Investment
import Hedger.Domain.InvestmentPerformance (
    InvestmentPerformanceContext,
    InvestmentSummary (..),
    InvestmentWarning,
    ScopeCache (..),
    SummaryResult (..),
    childrenOf,
    investmentAccountsAt,
    investmentDataTransactions,
    prepareInvestmentPerformance,
    reportPeriods,
    summariesAtPrepared,
    summariesAtResumed,
    summariesAtScopesPrepared,
    summariesAtScopesResumed,
    validateScope,
 )
import Hedger.Domain.Period (Granularity, epoch, lastMaybe)
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.DerivedRevision (DerivedCaches (..))

data InvestmentsRequest = InvestmentsRequest
    { granularity :: Granularity
    , displayCommodity :: Maybe Text
    , accountPrefix :: Maybe Text
    }

data InvestmentPoint = InvestmentPoint
    { period :: Text
    , startDate :: Day
    , summary :: InvestmentSummary
    }

data InvestmentBreadcrumb = InvestmentBreadcrumb
    { account :: Text
    , label :: Text
    }

data InvestmentChild = InvestmentChild
    { account :: Text
    , label :: Text
    , summary :: InvestmentSummary
    , hasChildren :: Bool
    , assetClass :: Investment.AssetClass
    , allocationPercentage :: Maybe Decimal
    }

data AssetClassAllocation = AssetClassAllocation
    { assetClass :: Investment.AssetClass
    , marketValue :: Maybe Decimal
    , currentPercentage :: Maybe Decimal
    , targetPercentage :: Maybe Decimal
    }

data InvestmentsReport = InvestmentsReport
    { metadata :: SnapshotMetadata
    , displayCommodity :: Text
    , granularity :: Granularity
    , accountPrefix :: Maybe Text
    , summary :: InvestmentSummary
    , timeline :: [InvestmentPoint]
    , breadcrumbs :: [InvestmentBreadcrumb]
    , children :: [InvestmentChild]
    , assetClassAllocations :: [AssetClassAllocation]
    , warnings :: [InvestmentWarning]
    }

{- | Assemble the investments screen payload from the domain calculation. The
calculation lives in "Hedger.Domain.InvestmentPerformance"; this use case only
resolves inputs, projects the results into UI read-models, and attaches
snapshot metadata.
-}
investments :: InvestmentsRequest -> AppM InvestmentsReport
investments request = do
    snapshotContext <- requireSnapshot
    responseMetadata <- currentMetadata snapshotContext
    config <- asks (.config)
    scope <- either throwError pure (validateScope config.investmentMappings request.accountPrefix)
    let snapshot = snapshotContext.snapshot
        commodity = fromMaybe config.defaultDisplayCommodity (request.displayCommodity >>= nonEmpty)
        latest = fromMaybe epoch snapshot.latestJournalDate
        performance =
            prepareInvestmentPerformance
                snapshot
                config.assetClassMappings
                config.investmentMappings
                commodity
        dataTransactions = investmentDataTransactions snapshot config.assetClassMappings scope
        periods = reportPeriods request.granularity dataTransactions latest
        investmentAccounts = investmentAccountsAt snapshot config.assetClassMappings scope latest
        childAccounts = childrenOf scope investmentAccounts
        allocationScopes =
            [ Just prefix
            | mapping <- Investment.assetClassMappingsList config.assetClassMappings
            , prefix <- mapping.accountPrefixes
            ]
        periodDays = List.nub [end | (_, _, end) <- periods]
        -- The selected scope is dropped when the timeline already covers the latest date,
        -- so `current` reuses that result instead of recomputing it on the singular path.
        summaryScopesFor periodSummaries =
            List.nub
                ( [scope | Map.notMember latest periodSummaries]
                    <> [Just child | child <- childAccounts]
                    <> [ allocationScope
                       | scope == Nothing
                       , allocationScope <- allocationScopes
                       ]
                )
    (periodSummaries, summaries) <-
        scopeSummaries
            snapshot
            performance
            scope
            request.granularity
            latest
            periodDays
            summaryScopesFor
    let summaryFor selectedScope = Map.lookup selectedScope summaries
        periodSummaryAt day =
            Map.lookup day periodSummaries
    current <-
        maybe
            (throwError InternalError)
            pure
            (Map.lookup latest periodSummaries <|> summaryFor scope)
    childResults <-
        traverse
            (\child -> maybe (throwError InternalError) pure (summaryFor (Just child)))
            childAccounts
    let currentSummary = current.summary
        hasInvestments =
            currentSummary.marketValue /= Nothing || currentSummary.purchaseCost /= Nothing
        points =
            if hasInvestments
                then
                    [ InvestmentPoint key start result.summary
                    | (key, start, end) <- periods
                    , Just result <- [periodSummaryAt end]
                    ]
                else []
        children = zipWith childReport childAccounts childResults
        allocations =
            if scope == Nothing
                then
                    assetClassAllocations
                        summaryFor
                        currentSummary.marketValue
                        config.assetClassMappings
                else []
        childReport child result =
            let childSummary = result.summary
             in InvestmentChild
                    { account = child
                    , label = accountLabel child
                    , summary = childSummary
                    , hasChildren =
                        not (null (childrenOf (Just child) investmentAccounts))
                    , assetClass = Investment.assetClassForAccount child config.assetClassMappings
                    , allocationPercentage =
                        allocationPercentage currentSummary.marketValue childSummary.marketValue
                    }
    pure
        InvestmentsReport
            { metadata = responseMetadata
            , displayCommodity = commodity
            , granularity = request.granularity
            , accountPrefix = scope
            , summary = currentSummary
            , timeline = points
            , breadcrumbs = breadcrumbsFor scope
            , children
            , assetClassAllocations = allocations
            , warnings = current.warnings
            }

{- | The timeline's per-period summaries and the singular summaries the screen
needs beside them, each scope's sweep resumed from its checkpoint when the
derived view describes this very snapshot and folded from the start of the
journal when it does not.

The two are computed together because the singular scopes depend on the
timeline: the selected scope is only asked for separately when the timeline did
not already cover the latest date. On the resumed path they also have to share a
'ScopeCache', so that the second sweep sees the checkpoints the first one laid
down and only one merged result is contributed back.
-}
scopeSummaries ::
    Snapshot ->
    InvestmentPerformanceContext ->
    Maybe Text ->
    Granularity ->
    Day ->
    [Day] ->
    (Map Day SummaryResult -> [Maybe Text]) ->
    AppM (Map Day SummaryResult, Map (Maybe Text) SummaryResult)
scopeSummaries snapshot performance scope granularity latest periodDays summaryScopesFor = do
    checkpoints <- usableCheckpoints snapshot
    case checkpoints of
        Nothing ->
            pure
                ( uncheckedPeriodSummaries
                , Map.fromList
                    ( summariesAtScopesPrepared
                        performance
                        latest
                        (summaryScopesFor uncheckedPeriodSummaries)
                    )
                )
        Just (chain, caches) -> do
            let (periodResults, afterPeriods) =
                    summariesAtResumed performance chain caches.scopes scope granularity periodDays
                periodSummaries = Map.fromList (zip periodDays periodResults)
                (scopeResults, afterScopes) =
                    summariesAtScopesResumed
                        performance
                        chain
                        afterPeriods
                        latest
                        (summaryScopesFor periodSummaries)
            contributeCheckpoints snapshot $ \current ->
                current{scopes = forcedScopeCache afterScopes}
            pure (periodSummaries, Map.fromList scopeResults)
  where
    uncheckedPeriodSummaries =
        Map.fromList (zip periodDays (summariesAtPrepared performance scope granularity periodDays))

{- | Force a request's scope checkpoints and solved rates before they reach the
derived view.

Each accumulator, scan and solved rate is forced as it is inserted, but the maps
themselves arrive as thunks; publishing them unforced would retain this request's
whole sweep and defer it into whichever later request first reads a checkpoint.
Reading the counts forces the insert folds, and with them every payload stored.
-}
forcedScopeCache :: ScopeCache -> ScopeCache
forcedScopeCache cache =
    checkpointCount cache.checkpoints `seq` solvedRateCount cache.rates `seq` cache

assetClassAllocations ::
    (Maybe Text -> Maybe SummaryResult) ->
    Maybe Decimal ->
    Investment.AssetClassMappings ->
    [AssetClassAllocation]
assetClassAllocations summaryFor total mappings =
    fmap allocationFor configuredClasses
  where
    configured = Investment.assetClassMappingsList mappings
    configuredClasses = List.nub (fmap (.assetClass) configured)
    allocationFor assetClass =
        let matching = filter ((== assetClass) . (.assetClass)) configured
            values =
                [ result.summary.marketValue
                | mapping <- matching
                , prefix <- mapping.accountPrefixes
                , Just result <- [summaryFor (Just prefix)]
                ]
            value =
                if null values
                    then Nothing
                    else Just (sum (fromMaybe 0 <$> values))
            target = firstJust ((.targetPercentage) <$> matching)
         in AssetClassAllocation
                { assetClass
                , marketValue = value
                , currentPercentage = allocationPercentage total value
                , targetPercentage = target
                }

firstJust :: [Maybe value] -> Maybe value
firstJust [] = Nothing
firstJust (Just value : _) = Just value
firstJust (Nothing : rest) = firstJust rest

breadcrumbsFor :: Maybe Text -> [InvestmentBreadcrumb]
breadcrumbsFor Nothing = []
breadcrumbsFor (Just scope) =
    [ InvestmentBreadcrumb (Text.intercalate ":" (take index parts)) part
    | (index, part) <- zip [1 ..] parts
    ]
  where
    parts = HledgerAccountName.accountNameComponents scope

accountLabel :: Text -> Text
accountLabel name = fromMaybe name (lastMaybe (HledgerAccountName.accountNameComponents name))

nonEmpty :: Text -> Maybe Text
nonEmpty value
    | Text.null (Text.strip value) = Nothing
    | otherwise = Just (Text.strip value)

allocationPercentage :: Maybe Decimal -> Maybe Decimal -> Maybe Decimal
allocationPercentage parentValue childValue = do
    total <- parentValue
    value <- childValue
    if abs total < 0.000001 then Nothing else Just (value / total * 100)
