module Hedger.Interface.HTTP.Endpoints.Investments (InvestmentsAPI, investmentsServer) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import Servant

import Hedger.Application.AppM (Env)
import Hedger.Application.Error (AppError)
import Hedger.Application.Investments qualified as Investments
import Hedger.Domain.Investment qualified as Investment
import Hedger.Domain.InvestmentPerformance qualified as Performance
import Hedger.Interface.HTTP.Core.Common (
    ResponseMetadata,
    decimalText,
    granularityText,
    investmentCompletenessText,
    parseGranularity,
    runAction,
    servantError,
    toResponseMetadata,
 )

-- DTO declaration

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
    , xirr :: Maybe Text
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
    , xirr :: Maybe Text
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
    , assetClass :: Text
    , allocationPercentage :: Maybe Text
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

data AssetClassAllocationDto = AssetClassAllocationDto
    { assetClass :: Text
    , marketValue :: Maybe Text
    , currentPercentage :: Maybe Text
    , targetPercentage :: Maybe Text
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
    , assetClassAllocations :: [AssetClassAllocationDto]
    , warnings :: [InvestmentWarningDto]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

-- API and server

type InvestmentsAPI =
    "investments" :> "query" :> ReqBody '[JSON] InvestmentsRequestDto :> Post '[JSON] InvestmentsResponse

investmentsServer :: Env -> Maybe Text -> Server InvestmentsAPI
investmentsServer env requestId request =
    case toRequest request of
        Left err -> throwError (servantError requestId err)
        Right parsed -> runAction env requestId (toResponse <$> Investments.investments parsed)

-- Mapper: domain <-> DTO

toRequest :: InvestmentsRequestDto -> Either AppError Investments.InvestmentsRequest
toRequest request = do
    granularity <- parseGranularity request.granularity
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
        , granularity = granularityText report.granularity
        , accountPrefix = report.accountPrefix
        , summary = toSummary report.summary
        , timeline = fmap toPoint report.timeline
        , breadcrumbs = fmap toBreadcrumb report.breadcrumbs
        , children = fmap toChild report.children
        , assetClassAllocations = fmap toAssetClassAllocation report.assetClassAllocations
        , warnings = fmap toWarning report.warnings
        }

toSummary :: Performance.InvestmentSummary -> InvestmentSummaryDto
toSummary value =
    InvestmentSummaryDto
        { marketValue = decimalText <$> value.marketValue
        , purchaseCost = decimalText <$> value.purchaseCost
        , gainLoss = decimalText <$> value.gainLoss
        , xirr = decimalText <$> value.xirr
        , attributedIncome = decimalText <$> value.attributedIncome
        , realisedGain = decimalText <$> value.realisedGain
        , totalProfit = decimalText <$> value.totalProfit
        , completeness = investmentCompletenessText value.completeness
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
            , xirr = summary.xirr
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
        , assetClass = assetClassText value.assetClass
        , allocationPercentage = decimalText <$> value.allocationPercentage
        }

toAssetClassAllocation :: Investments.AssetClassAllocation -> AssetClassAllocationDto
toAssetClassAllocation value =
    AssetClassAllocationDto
        { assetClass = assetClassText value.assetClass
        , marketValue = decimalText <$> value.marketValue
        , currentPercentage = decimalText <$> value.currentPercentage
        , targetPercentage = decimalText <$> value.targetPercentage
        }

assetClassText :: Investment.AssetClass -> Text
assetClassText assetClass = case assetClass of
    Investment.Equity -> "equity"
    Investment.Debt -> "debt"
    Investment.Cash -> "cash"
    Investment.Gold -> "gold"
    Investment.Hybrid -> "hybrid"
    Investment.Property -> "property"
    Investment.Other -> "other"

toWarning :: Performance.InvestmentWarning -> InvestmentWarningDto
toWarning (Performance.InvestmentWarning kind account) =
    let (kindText, message) = renderWarning kind
     in InvestmentWarningDto{kind = kindText, account = account, message = message}

renderWarning :: Performance.WarningKind -> (Text, Text)
renderWarning kind = case kind of
    Performance.MissingPrice commodity symbol ->
        ("missing-price", "No " <> commodity <> " price for " <> symbol <> ".")
    Performance.MissingCost symbol ->
        ("missing-cost", "No complete acquisition cost for " <> symbol <> ".")
    Performance.MissingPerformanceMapping ->
        ( "missing-performance-mapping"
        , "Income and realised gains are unavailable because an investment account mapping is missing."
        )
    Performance.ForeignCost commodity symbol ->
        ( "foreign-cost"
        , "Acquisition cost in " <> symbol <> " cannot be reported as historical " <> commodity <> " cost."
        )
    Performance.UnresolvedReturn ->
        ( "unresolved-return"
        , "The internal rate of return could not be resolved (no unique or convergent rate)."
        )
