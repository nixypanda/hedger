module Hedger.Application.Common (
    SnapshotMetadata (..),
    metadataFor,
    AccountTypeClassifier,
    accountTypeClassifier,
)
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (Text)

import Hledger.Data.AccountName qualified as HledgerAccountName
import Hledger.Data.Types qualified as Hledger

import Hedger.Application.AppM (AppM, Env (..))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.SnapshotStore (readLastReloadError)

{- | Classify an account name into its hledger 'Hledger.AccountType', honouring
both explicit journal @type:@ declarations and hledger's built-in regex
inference for common top-level account names. This is the single source of
truth for account-type classification across the API surface.
-}
type AccountTypeClassifier = Text -> Maybe Hledger.AccountType

accountTypeClassifier :: Hledger.Journal -> AccountTypeClassifier
accountTypeClassifier journal = HledgerAccountName.accountNameType journal.jaccounttypes

data SnapshotMetadata = SnapshotMetadata
    { revision :: Text
    , freshness :: Text
    , warnings :: [Text]
    }

metadataFor :: Snapshot -> AppM SnapshotMetadata
metadataFor snapshot = do
    store <- asks (.snapshotStore)
    reloadError <- liftIO (readLastReloadError store)
    pure
        SnapshotMetadata
            { revision = snapshot.revision
            , freshness = maybe "fresh" (const "stale") reloadError
            , warnings =
                maybe
                    []
                    (const ["The latest journal reload failed; the last usable snapshot is shown."])
                    reloadError
            }
