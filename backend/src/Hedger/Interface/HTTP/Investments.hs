module Hedger.Interface.HTTP.Investments (InvestmentsAPI, investmentsServer) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Decimal (Decimal)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)
import GHC.Generics (Generic)
import Servant

import Hedger.Application.Analytics qualified as Analytics
import Hedger.Application.AppM (Env)
import Hedger.Application.Investments qualified as Investments
import Hedger.Domain.Error (AppError (..))
import Hedger.Interface.HTTP.Common (ResponseMetadata, runAction, servantError, toResponseMetadata)

data InvestmentsRequestDto = InvestmentsRequestDto
    { granularity :: Text
    , displayCommodity :: Maybe Text
    , accountPrefix :: Maybe Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

data InvestmentSummaryDto = InvestmentSummaryDto
    { marketValue :: Maybe Text
    , purchaseCost :: Maybe Text
    , gainLoss :: Maybe Text
    , returnPercentage :: Maybe Text
    , attributedIncome :: Maybe Text
    , realisedGain :: Maybe Text
    , totalProfit :: Maybe Text
    , completeness :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

data InvestmentPointDto = InvestmentPointDto
    { period :: Text
    , startDate :: Day
    , marketValue :: Maybe Text
    , purchaseCost :: Maybe Text
    , gainLoss :: Maybe Text
    , returnPercentage :: Maybe Text
    , attributedIncome :: Maybe Text
    , realisedGain :: Maybe Text
    , totalProfit :: Maybe Text
    , completeness :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

data InvestmentBreadcrumbDto = InvestmentBreadcrumbDto
    { account :: Text
    , label :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

data InvestmentChildDto = InvestmentChildDto
    { account :: Text
    , label :: Text
    , summary :: InvestmentSummaryDto
    , hasChildren :: Bool
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

data InvestmentWarningDto = InvestmentWarningDto
    { kind :: Text
    , account :: Maybe Text
    , message :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

data InvestmentsResponse = InvestmentsResponse
    { metadata :: ResponseMetadata
    , displayCommodity :: Text
    , granularity :: Text
    , accountPrefix :: Maybe Text
    , summary :: InvestmentSummaryDto
    , timeline :: [InvestmentPointDto]
    , breadcrumbs :: [InvestmentBreadcrumbDto]
    , children :: [InvestmentChildDto]
    , warnings :: [InvestmentWarningDto]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

type InvestmentsAPI =
    "investments" :> "query" :> ReqBody '[JSON] InvestmentsRequestDto :> Post '[JSON] InvestmentsResponse

investmentsServer :: Env -> Maybe Text -> Server InvestmentsAPI
investmentsServer env requestId request =
    case toRequest request of
        Left err -> throwError (servantError requestId err)
        Right parsed -> runAction env requestId (toResponse <$> Investments.investments parsed)

toRequest :: InvestmentsRequestDto -> Either AppError Investments.InvestmentsRequest
toRequest request = do
    granularity <- case Text.toLower request.granularity of
        "monthly" -> Right Analytics.Monthly
        "yearly" -> Right Analytics.Yearly
        _ -> Left (InvalidRequest "Granularity must be monthly or yearly.")
    pure
        Investments.InvestmentsRequest
            { granularity
            , displayCommodity = request.displayCommodity
            , accountPrefix = request.accountPrefix
            }

toResponse :: Investments.InvestmentsReport -> InvestmentsResponse
toResponse report =
    InvestmentsResponse
        { metadata = toResponseMetadata report.metadata
        , displayCommodity = report.displayCommodity
        , granularity = case report.granularity of
            Analytics.Monthly -> "monthly"
            Analytics.Yearly -> "yearly"
        , accountPrefix = report.accountPrefix
        , summary = toSummary report.summary
        , timeline = fmap toPoint report.timeline
        , breadcrumbs = fmap toBreadcrumb report.breadcrumbs
        , children = fmap toChild report.children
        , warnings = fmap toWarning report.warnings
        }

toSummary :: Investments.InvestmentSummary -> InvestmentSummaryDto
toSummary value =
    InvestmentSummaryDto
        { marketValue = decimal <$> value.marketValue
        , purchaseCost = decimal <$> value.purchaseCost
        , gainLoss = decimal <$> value.gainLoss
        , returnPercentage = decimal <$> value.returnPercentage
        , attributedIncome = decimal <$> value.attributedIncome
        , realisedGain = decimal <$> value.realisedGain
        , totalProfit = decimal <$> value.totalProfit
        , completeness = case value.completeness of
            Investments.Complete -> "complete"
            Investments.Partial -> "partial"
            Investments.Unavailable -> "unavailable"
        }

toPoint :: Investments.InvestmentPoint -> InvestmentPointDto
toPoint value =
    let summary = toSummary value.summary
     in InvestmentPointDto
            { period = value.period
            , startDate = value.startDate
            , marketValue = summary.marketValue
            , purchaseCost = summary.purchaseCost
            , gainLoss = summary.gainLoss
            , returnPercentage = summary.returnPercentage
            , attributedIncome = summary.attributedIncome
            , realisedGain = summary.realisedGain
            , totalProfit = summary.totalProfit
            , completeness = summary.completeness
            }

toBreadcrumb :: Investments.InvestmentBreadcrumb -> InvestmentBreadcrumbDto
toBreadcrumb value = InvestmentBreadcrumbDto{account = value.account, label = value.label}

toChild :: Investments.InvestmentChild -> InvestmentChildDto
toChild value =
    InvestmentChildDto
        { account = value.account
        , label = value.label
        , summary = toSummary value.summary
        , hasChildren = value.hasChildren
        }

toWarning :: Investments.InvestmentWarning -> InvestmentWarningDto
toWarning value = InvestmentWarningDto{kind = value.kind, account = value.account, message = value.message}

decimal :: Decimal -> Text
decimal = Text.pack . show
