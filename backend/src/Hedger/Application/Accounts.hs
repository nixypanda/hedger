module Hedger.Application.Accounts (
    AccountTreeNode (..),
    AccountsTree (..),
    AccountRegister (..),
    accountsTree,
    accountRegister,
)
where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, fromGregorian)

import Hledger.Data.AccountName qualified as HledgerAccountName
import Hledger.Data.Types qualified as Hledger

import Hedger.Application.AccountRegisterQuery (RegisterPageRequest (..))
import Hedger.Application.AccountRegisterQuery qualified as AccountRegisterQuery
import Hedger.Application.AppM (AppM, Env (..), SnapshotContext (..), requireSnapshot)
import Hedger.Application.Common (SnapshotMetadata, metadataFor)
import Hedger.Config (Config (..))
import Hedger.Domain.Account qualified as Account
import Hedger.Domain.Snapshot (Snapshot (..))

data AccountTreeNode = AccountTreeNode
    { path :: Text
    , name :: Text
    , accountType :: Maybe Hledger.AccountType
    , hasChildren :: Bool
    , directBalances :: Hledger.MixedAmount
    , subtreeBalances :: Hledger.MixedAmount
    , valuation :: Account.ValuedBalances
    , children :: [AccountTreeNode]
    }

data AccountsTree = AccountsTree
    { metadata :: SnapshotMetadata
    , accounts :: [AccountTreeNode]
    , displayCommodity :: Text
    , valuationDate :: Day
    }

data AccountRegister = AccountRegister
    { metadata :: SnapshotMetadata
    , displayCommodity :: Text
    , entries :: [AccountRegisterQuery.RegisterEntry]
    , nextCursor :: Maybe Text
    }

accountsTree :: Maybe Text -> AppM AccountsTree
accountsTree requestedCommodity = do
    snapshotContext <- requireSnapshot
    configuredCommodity <- asks (.config.defaultDisplayCommodity)
    let snapshot = snapshotContext.snapshot
        responseMetadata = metadataFor snapshotContext
        displayCommodity = fromMaybe configuredCommodity (nonEmpty requestedCommodity)
        valuationDate = fromMaybe (fromGregorian 1970 1 1) snapshot.latestJournalDate
        valuationContext = Account.mkValuationContext snapshot.journal displayCommodity valuationDate
    pure
        AccountsTree
            { metadata = responseMetadata
            , accounts = toAccountTreeNode snapshot.journal valuationContext <$> snapshot.accountTree.asubs
            , displayCommodity
            , valuationDate
            }

toAccountTreeNode ::
    Hledger.Journal ->
    Account.ValuationContext ->
    Hledger.Account Hledger.BalanceData ->
    AccountTreeNode
toAccountTreeNode journal valuationContext account =
    AccountTreeNode
        { path = account.aname
        , name = accountLeafName account.aname
        , accountType = HledgerAccountName.accountNameType journal.jaccounttypes account.aname
        , hasChildren = not (null account.asubs)
        , directBalances = direct
        , subtreeBalances = subtree
        , valuation = Account.valueBalances valuationContext direct subtree
        , children = toAccountTreeNode journal valuationContext <$> account.asubs
        }
  where
    direct = Hledger.bdexcludingsubs (totalBalanceData account.adata)
    subtree = Hledger.bdincludingsubs (totalBalanceData account.adata)

totalBalanceData :: Hledger.PeriodData Hledger.BalanceData -> Hledger.BalanceData
totalBalanceData periodData = foldMap id (periodData.pdpre : Map.elems periodData.pdperiods)

accountLeafName :: Text -> Text
accountLeafName name = case Text.splitOn ":" name of
    [] -> name
    components -> last components

accountRegister :: RegisterPageRequest -> AppM AccountRegister
accountRegister request = do
    snapshotContext <- requireSnapshot
    configuredCommodity <- asks (.config.defaultDisplayCommodity)
    let snapshot = snapshotContext.snapshot
        responseMetadata = metadataFor snapshotContext
        resolvedCommodity = fromMaybe configuredCommodity (nonEmpty request.commodity)
    page <-
        either throwError pure $
            AccountRegisterQuery.registerPage snapshot resolvedCommodity request
    pure
        AccountRegister
            { metadata = responseMetadata
            , displayCommodity = resolvedCommodity
            , entries = page.entries
            , nextCursor = page.nextCursor
            }

nonEmpty :: Maybe Text -> Maybe Text
nonEmpty value = do
    text <- value
    if Text.null text then Nothing else Just text
