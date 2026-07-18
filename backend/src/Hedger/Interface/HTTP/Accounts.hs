module Hedger.Interface.HTTP.Accounts (
    AccountsAPI,
    accountsServer,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)
import GHC.Generics (Generic)
import Servant

import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Types qualified as Hledger

import Hedger.Application.AccountRegisterQuery (RegisterEntry (..), RegisterPageRequest (..))
import Hedger.Application.Accounts (
    AccountRegister (..),
    AccountTreeNode (..),
    AccountsTree (..),
    accountRegister,
    accountsTree,
 )
import Hedger.Application.AppM (Env)
import Hedger.Domain.Account qualified as Account
import Hedger.Domain.Error (AppError)
import Hedger.Interface.HTTP.Common (
    CommodityQuantity (..),
    ResponseMetadata (..),
    parseCanonicalAccountPath,
    parseDay,
    parseLimit,
    runAction,
    servantError,
    toResponseMetadata,
    validateDateRange,
 )

-- DTO declaration

data AccountValuationDto = AccountValuationDto
    { asOf :: Day
    , commodity :: Text
    , directBalances :: [CommodityQuantity]
    , subtreeBalances :: [CommodityQuantity]
    , completeness :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data AccountNodeDto = AccountNodeDto
    { path :: Text
    , name :: Text
    , accountType :: Maybe Text
    , hasChildren :: Bool
    , directBalances :: [CommodityQuantity]
    , subtreeBalances :: [CommodityQuantity]
    , valuation :: AccountValuationDto
    , children :: [AccountNodeDto]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data AccountsTreeResponse = AccountsTreeResponse
    { metadata :: ResponseMetadata
    , displayCommodity :: Text
    , accounts :: [AccountNodeDto]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data RegisterValueDto = RegisterValueDto
    { raw :: [CommodityQuantity]
    , display :: [CommodityQuantity]
    , completeness :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data RegisterEntryDto = RegisterEntryDto
    { date :: Day
    , description :: Text
    , account :: Text
    , amount :: RegisterValueDto
    , balance :: RegisterValueDto
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data AccountRegisterResponse = AccountRegisterResponse
    { metadata :: ResponseMetadata
    , displayCommodity :: Text
    , entries :: [RegisterEntryDto]
    , nextCursor :: Maybe Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- API and server

type AccountsAPI =
    "accounts" :> "tree" :> QueryParam "commodity" Text :> Get '[JSON] AccountsTreeResponse
        :<|> ( "accounts"
                :> Capture "account" Text
                :> "register"
                :> QueryParam "commodity" Text
                :> QueryParam "from" Text
                :> QueryParam "to" Text
                :> QueryParam "limit" Text
                :> QueryParam "cursor" Text
                :> Get '[JSON] AccountRegisterResponse
             )

accountsServer :: Env -> Maybe Text -> Server AccountsAPI
accountsServer env requestId =
    (\commodity -> runAction env requestId (toAccountsTree <$> accountsTree commodity))
        :<|> ( \account commodity from to limit cursor ->
                case mkRegisterPageRequest account commodity from to limit cursor of
                    Left err -> throwError (servantError requestId err)
                    Right request ->
                        runAction env requestId (toAccountRegisterResponse <$> accountRegister request)
             )

mkRegisterPageRequest ::
    Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    Either AppError RegisterPageRequest
mkRegisterPageRequest account commodity from to limit cursor = do
    accountPath <- parseCanonicalAccountPath account
    start <- parseDay from
    end <- parseDay to
    _ <- validateDateRange start end
    limitValue <- parseLimit limit
    pure $ RegisterPageRequest{account = accountPath, commodity, start, end, limit = limitValue, cursor}

-- Mapper: domain <-> DTO

toAccountsTree :: AccountsTree -> AccountsTreeResponse
toAccountsTree value =
    AccountsTreeResponse
        { metadata = toResponseMetadata value.metadata
        , displayCommodity = value.displayCommodity
        , accounts = toAccountNode value.displayCommodity value.valuationDate <$> value.accounts
        }

toAccountNode :: Text -> Day -> AccountTreeNode -> AccountNodeDto
toAccountNode displayCommodity valuationDate account =
    AccountNodeDto
        { path = account.path
        , name = account.name
        , accountType =
            accountTypeText <$> account.accountType
        , hasChildren = account.hasChildren
        , directBalances = amounts account.directBalances
        , subtreeBalances = amounts account.subtreeBalances
        , valuation = valuationDto displayCommodity valuationDate account.valuation
        , children = toAccountNode displayCommodity valuationDate <$> account.children
        }

amounts :: Hledger.MixedAmount -> [CommodityQuantity]
amounts = fmap toCommodityQuantity . HledgerAmount.amountsRaw

valuationDto :: Text -> Day -> Account.ValuedBalances -> AccountValuationDto
valuationDto displayCommodity valuationDate valued =
    AccountValuationDto
        { asOf = valuationDate
        , commodity = displayCommodity
        , directBalances = amounts valued.directBalance
        , subtreeBalances = amounts valued.subtreeBalance
        , completeness = completenessText valued.completeness
        }

completenessText :: Account.ValuationCompleteness -> Text
completenessText status = case status of
    Account.Complete -> "complete"
    Account.Partial -> "partial"
    Account.Unavailable -> "unavailable"

toCommodityQuantity :: Hledger.Amount -> CommodityQuantity
toCommodityQuantity amount =
    CommodityQuantity
        { commodity = amount.acommodity
        , quantity = Text.pack (show amount.aquantity)
        }

accountTypeText :: Hledger.AccountType -> Text
accountTypeText accountType = case accountType of
    Hledger.Asset -> "asset"
    Hledger.Liability -> "liability"
    Hledger.Equity -> "equity"
    Hledger.Revenue -> "income"
    Hledger.Expense -> "expense"
    Hledger.Cash -> "cash"
    Hledger.Conversion -> "conversion"
    Hledger.Gain -> "gain"

toAccountRegisterResponse :: AccountRegister -> AccountRegisterResponse
toAccountRegisterResponse value =
    AccountRegisterResponse
        { metadata = toResponseMetadata value.metadata
        , displayCommodity = value.displayCommodity
        , entries = toRegisterEntries value.displayCommodity value.entries
        , nextCursor = value.nextCursor
        }

toRegisterEntries :: Text -> [RegisterEntry] -> [RegisterEntryDto]
toRegisterEntries displayCommodity = fmap (toRegisterEntry displayCommodity)

toRegisterEntry :: Text -> RegisterEntry -> RegisterEntryDto
toRegisterEntry displayCommodity RegisterEntry{rawItem = raw, displayItem = valued, matchedAccounts} =
    RegisterEntryDto
        { date = valuedTransaction.tdate
        , description = valuedTransaction.tdescription
        , account = Text.intercalate ", " matchedAccounts
        , amount = valueDto displayCommodity rawAmount valuedAmount
        , balance = valueDto displayCommodity rawBalance valuedBalance
        }
  where
    (_, valuedTransaction, _, _, rawAmount, rawBalance) = raw
    (_, _, _, _, valuedAmount, valuedBalance) = valued

valueDto :: Text -> Hledger.MixedAmount -> Hledger.MixedAmount -> RegisterValueDto
valueDto displayCommodity rawAmount valuedAmount =
    RegisterValueDto
        { raw = amounts rawAmount
        , display = amounts valuedAmount
        , completeness = completenessForRaw displayCommodity valuedAmount
        }

completenessForRaw :: Text -> Hledger.MixedAmount -> Text
completenessForRaw displayCommodity valuedAmount
    | all ((== displayCommodity) . (.commodity)) projected = "complete"
    | any ((== displayCommodity) . (.commodity)) projected = "partial"
    | otherwise = "unavailable"
  where
    projected = amounts valuedAmount
