module Hedger.Application.Investments (
    InvestmentsRequest (..),
    InvestmentPoint (..),
    InvestmentBreadcrumb (..),
    InvestmentChild (..),
    AssetClassAllocation (..),
    InvestmentsReport (..),
    investments,
) where

import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Decimal (Decimal)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)

import Hledger.Data.AccountName qualified as HledgerAccountName

import Hedger.Application.AppM (AppM, Env (..), SnapshotContext (..), requireSnapshot)
import Hedger.Application.Common (SnapshotMetadata, metadataFor)
import Hedger.Config (Config (..))
import Hedger.Domain.Error (AppError (InternalError))
import Hedger.Domain.Investment qualified as Investment
import Hedger.Domain.InvestmentPerformance (
    InvestmentWarning,
    SummaryResult,
    childrenOf,
    investmentAccountsAt,
    investmentDataTransactions,
    prepareInvestmentPerformance,
    reportPeriods,
    summariesAtPrepared,
    summariesAtScopesPrepared,
    summaryMarketValue,
    summaryPurchaseCost,
    summaryWarnings,
    validateScope,
 )
import Hedger.Domain.Period (Granularity, epoch, lastMaybe)
import Hedger.Domain.Snapshot (Snapshot (..))

data InvestmentsRequest = InvestmentsRequest
    { granularity :: Granularity
    , displayCommodity :: Maybe Text
    , accountPrefix :: Maybe Text
    }

data InvestmentPoint = InvestmentPoint
    { period :: Text
    , startDate :: Day
    , summary :: SummaryResult
    }

data InvestmentBreadcrumb = InvestmentBreadcrumb
    { account :: Text
    , label :: Text
    }

data InvestmentChild = InvestmentChild
    { account :: Text
    , label :: Text
    , summary :: SummaryResult
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
    , summary :: SummaryResult
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
    config <- asks (.config)
    scope <- either throwError pure (validateScope config.investmentMappings request.accountPrefix)
    let snapshot = snapshotContext.snapshot
        responseMetadata = metadataFor snapshotContext
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
            [ Just (Investment.assetAccountText prefix)
            | mapping <- Investment.assetClassMappingsList config.assetClassMappings
            , prefix <- NonEmpty.toList mapping.accountPrefixes
            ]
        periodDays = List.nub [end | (_, _, end) <- periods]
        periodSummaries =
            Map.fromList
                ( zip
                    periodDays
                    (summariesAtPrepared performance scope request.granularity periodDays)
                )
        summaryScopes =
            List.nub
                ( [scope | Map.notMember latest periodSummaries]
                    <> [Just child | child <- childAccounts]
                    <> [allocationScope | scope == Nothing, allocationScope <- allocationScopes]
                )
        summaries = Map.fromList (summariesAtScopesPrepared performance latest summaryScopes)
        summaryFor selectedScope = Map.lookup selectedScope summaries
        periodSummaryAt day = Map.lookup day periodSummaries
    -- The selected scope is dropped when the timeline already covers the latest date,
    -- so `current` reuses that result instead of recomputing it on the singular path.
    current <-
        maybe
            (throwError InternalError)
            pure
            (Map.lookup latest periodSummaries <|> summaryFor scope)
    childResults <-
        traverse
            (\child -> maybe (throwError InternalError) pure (summaryFor (Just child)))
            childAccounts
    let currentSummary = current
        hasInvestments =
            summaryMarketValue current /= Nothing || summaryPurchaseCost current /= Nothing
        points =
            if hasInvestments
                then
                    [ InvestmentPoint key start result
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
                        (summaryMarketValue current)
                        config.assetClassMappings
                else []
        childReport child result =
            let childSummary = result
             in InvestmentChild
                    { account = child
                    , label = accountLabel child
                    , summary = childSummary
                    , hasChildren = not (null (childrenOf (Just child) investmentAccounts))
                    , assetClass = Investment.assetClassForAccount child config.assetClassMappings
                    , allocationPercentage =
                        allocationPercentage
                            (summaryMarketValue current)
                            (summaryMarketValue childSummary)
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
            , warnings = summaryWarnings current
            }

assetClassAllocations ::
    (Maybe Text -> Maybe SummaryResult) ->
    Maybe Decimal ->
    Investment.AssetClassMappings ->
    [AssetClassAllocation]
assetClassAllocations summaryFor total mappings = fmap allocationFor configuredClasses
  where
    configured = Investment.assetClassMappingsList mappings
    configuredClasses = List.nub (fmap (.assetClass) configured)
    allocationFor assetClass =
        let matching = filter ((== assetClass) . (.assetClass)) configured
            values =
                [ summaryMarketValue result
                | mapping <- matching
                , prefix <- NonEmpty.toList mapping.accountPrefixes
                , Just result <- [summaryFor (Just (Investment.assetAccountText prefix))]
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
                , targetPercentage = Investment.percentageValue <$> target
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
    if abs total < 0.000001
        then Nothing
        else Just (value / total * 100)
