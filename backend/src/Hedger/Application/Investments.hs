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

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Decimal (Decimal)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)

import Hledger.Data.AccountName qualified as HledgerAccountName

import Hedger.Application.AppM (AppM, Env (..), SnapshotContext (..), requireSnapshot)
import Hedger.Application.Common (SnapshotMetadata, metadataFor)
import Hedger.Config (Config (..))
import Hedger.Domain.Investment qualified as Investment
import Hedger.Domain.InvestmentPerformance (
    InvestmentSummary (..),
    InvestmentWarning,
    SummaryResult (..),
    childrenOf,
    investmentAccountsAt,
    investmentDataTransactions,
    prepareInvestmentPerformance,
    reportPeriods,
    summaryAtPrepared,
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
            [ Just prefix
            | mapping <- Investment.assetClassMappingsList config.assetClassMappings
            , prefix <- mapping.accountPrefixes
            ]
        summaryKeys =
            List.nub
                ( (scope, latest)
                    : [(scope, end) | (_, _, end) <- periods]
                        <> [(Just child, latest) | child <- childAccounts]
                        <> [ (allocationScope, latest)
                           | scope == Nothing
                           , allocationScope <- allocationScopes
                           ]
                )
        summaries =
            Map.fromList
                [ (key, summaryAtPrepared performance selectedScope selectedDay)
                | key@(selectedScope, selectedDay) <- summaryKeys
                ]
        summaryFor selectedScope selectedDay =
            fromMaybe
                (summaryAtPrepared performance selectedScope selectedDay)
                (Map.lookup (selectedScope, selectedDay) summaries)
        current = summaryFor scope latest
        hasInvestments = current.summary.marketValue /= Nothing || current.summary.purchaseCost /= Nothing
        points =
            if hasInvestments
                then
                    [ InvestmentPoint key start (summaryFor scope end).summary
                    | (key, start, end) <- periods
                    ]
                else []
        children = fmap childReport childAccounts
        allocations =
            if scope == Nothing
                then
                    assetClassAllocations
                        summaryFor
                        latest
                        current.summary.marketValue
                        config.assetClassMappings
                else []
        childReport child =
            let childSummary = (summaryFor (Just child) latest).summary
             in InvestmentChild
                    { account = child
                    , label = accountLabel child
                    , summary = childSummary
                    , hasChildren =
                        not (null (childrenOf (Just child) investmentAccounts))
                    , assetClass = Investment.assetClassForAccount child config.assetClassMappings
                    , allocationPercentage =
                        allocationPercentage current.summary.marketValue childSummary.marketValue
                    }
    pure
        InvestmentsReport
            { metadata = responseMetadata
            , displayCommodity = commodity
            , granularity = request.granularity
            , accountPrefix = scope
            , summary = current.summary
            , timeline = points
            , breadcrumbs = breadcrumbsFor scope
            , children
            , assetClassAllocations = allocations
            , warnings = current.warnings
            }

assetClassAllocations ::
    (Maybe Text -> Day -> SummaryResult) ->
    Day ->
    Maybe Decimal ->
    Investment.AssetClassMappings ->
    [AssetClassAllocation]
assetClassAllocations summaryFor latest total mappings =
    fmap allocationFor configuredClasses
  where
    configured = Investment.assetClassMappingsList mappings
    configuredClasses = List.nub (fmap (.assetClass) configured)
    allocationFor assetClass =
        let matching = filter ((== assetClass) . (.assetClass)) configured
            values =
                [ (summaryFor (Just prefix) latest).summary.marketValue
                | mapping <- matching
                , prefix <- mapping.accountPrefixes
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
