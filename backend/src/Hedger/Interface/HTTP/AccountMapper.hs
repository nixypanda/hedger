module Hedger.Interface.HTTP.AccountMapper (toAccountsTree) where

import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day)
import Hedger.Application.AppM (AccountsTree (..), SnapshotMetadata (..))
import Hedger.Interface.HTTP.Types (AccountNodeDto (..), AccountValuationDto (..), AccountsTreeResponse (..), CommodityQuantity (..), ResponseMetadata (..))
import Hledger.Data.AccountName qualified as HledgerAccountName
import Hledger.Data.Amount qualified as HledgerAmount
import Hledger.Data.Journal qualified as HledgerJournal
import Hledger.Data.Types qualified as Hledger
import Hledger.Data.Valuation qualified as HledgerValuation

toAccountsTree :: AccountsTree -> AccountsTreeResponse
toAccountsTree value =
    AccountsTreeResponse
        { metadata = metadataDto value.metadata
        , displayCommodity = value.displayCommodity
        , accounts = toAccountNode value.journal value.displayCommodity value.valuationDate <$> value.tree.asubs
        }

metadataDto :: SnapshotMetadata -> ResponseMetadata
metadataDto (SnapshotMetadata revisionValue freshnessValue warningValues) =
    ResponseMetadata
        { revision = revisionValue
        , freshness = freshnessValue
        , warnings = warningValues
        }

toAccountNode :: Hledger.Journal -> Text -> Day -> Hledger.Account Hledger.BalanceData -> AccountNodeDto
toAccountNode journal displayCommodity valuationDate account =
    AccountNodeDto
        { path = account.aname
        , name = accountLeafName account.aname
        , accountType = accountTypeText <$> HledgerAccountName.accountNameType journal.jaccounttypes account.aname
        , hasChildren = not (null account.asubs)
        , directBalances = amounts direct
        , subtreeBalances = amounts subtree
        , valuation = valuationDto journal displayCommodity valuationDate direct subtree
        , children = toAccountNode journal displayCommodity valuationDate <$> account.asubs
        }
  where
    direct = Hledger.bdexcludingsubs (totalBalanceData account.adata)
    subtree = Hledger.bdincludingsubs (totalBalanceData account.adata)

totalBalanceData :: Hledger.PeriodData Hledger.BalanceData -> Hledger.BalanceData
totalBalanceData periodData = foldMap id (periodData.pdpre : Map.elems periodData.pdperiods)

amounts :: Hledger.MixedAmount -> [CommodityQuantity]
amounts = fmap toCommodityQuantity . HledgerAmount.amountsRaw

valuationDto :: Hledger.Journal -> Text -> Day -> Hledger.MixedAmount -> Hledger.MixedAmount -> AccountValuationDto
valuationDto journal displayCommodity valuationDate direct subtree =
    AccountValuationDto
        { asOf = valuationDate
        , commodity = displayCommodity
        , directBalances = amounts valuedDirect
        , subtreeBalances = amounts valuedSubtree
        , completeness = completenessFor displayCommodity valuedDirect valuedSubtree
        }
  where
    oracle = HledgerValuation.journalPriceOracle False journal
    styles = HledgerJournal.journalCommodityStyles journal
    value amount = HledgerValuation.mixedAmountValueAtDate oracle styles (Just displayCommodity) valuationDate amount
    valuedDirect = value direct
    valuedSubtree = value subtree

    completenessFor target directAmount subtreeAmount
        | all (isTarget target) (amounts directAmount <> amounts subtreeAmount) = "complete"
        | any (isTarget target) (amounts directAmount <> amounts subtreeAmount) = "partial"
        | otherwise = "unavailable"

    isTarget target amount = amount.commodity == target

toCommodityQuantity :: Hledger.Amount -> CommodityQuantity
toCommodityQuantity amount =
    CommodityQuantity
        { commodity = amount.acommodity
        , quantity = Text.pack (show amount.aquantity)
        }

accountLeafName :: Text -> Text
accountLeafName name = case Text.splitOn ":" name of
    [] -> name
    components -> last components

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
