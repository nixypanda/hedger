module Hedger.Application.Investments (
    InvestmentsRequest (..),
    InvestmentPoint (..),
    InvestmentBreadcrumb (..),
    InvestmentChild (..),
    InvestmentsReport (..),
    investments,
)
where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)

import Hledger.Data.AccountName qualified as HledgerAccountName

import Hedger.Application.AppM (AppM, Env (..), requireSnapshot)
import Hedger.Application.Common (SnapshotMetadata, metadataFor)
import Hedger.Config (Config (..))
import Hedger.Domain.InvestmentPerformance (
    InvestmentSummary (..),
    InvestmentWarning,
    SummaryResult (..),
    immediateChildren,
    investmentDataTransactions,
    reportPeriods,
    summaryAt,
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
    , warnings :: [InvestmentWarning]
    }

{- | Assemble the investments screen payload from the domain calculation. The
calculation lives in "Hedger.Domain.InvestmentPerformance"; this use case only
resolves inputs, projects the results into UI read-models, and attaches
snapshot metadata.
-}
investments :: InvestmentsRequest -> AppM InvestmentsReport
investments request = do
    snapshot <- requireSnapshot
    config <- asks (.config)
    scope <- either throwError pure (validateScope config.investmentMappings request.accountPrefix)
    responseMetadata <- metadataFor snapshot
    let commodity = fromMaybe config.defaultDisplayCommodity (request.displayCommodity >>= nonEmpty)
        latest = fromMaybe epoch snapshot.latestJournalDate
        summaryFor selectedScope selectedDay =
            summaryAt snapshot config.investmentMappings commodity selectedScope selectedDay
        current = summaryFor scope latest
        hasInvestments = current.summary.marketValue /= Nothing || current.summary.purchaseCost /= Nothing
        dataTransactions = investmentDataTransactions snapshot config.investmentMappings scope
        points =
            if hasInvestments
                then
                    [ InvestmentPoint key start (summaryFor scope end).summary
                    | (key, start, end) <- reportPeriods request.granularity dataTransactions latest
                    ]
                else []
        childAccounts = immediateChildren snapshot config.investmentMappings scope
        children =
            [ InvestmentChild
                { account = child
                , label = accountLabel child
                , summary = (summaryFor (Just child) latest).summary
                , hasChildren = not (null (immediateChildren snapshot config.investmentMappings (Just child)))
                }
            | child <- childAccounts
            ]
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
            , warnings = current.warnings
            }

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
