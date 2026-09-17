module Hedger.Application.Common (
    SnapshotMetadata (..),
    metadataFor,
    AccountTypeClassifier,
    accountTypeClassifier,
) where

import Data.Text (Text)

import Hledger.Data.AccountName qualified as HledgerAccountName
import Hledger.Data.Types qualified as Hledger

import Hedger.Application.AppM (SnapshotContext (..))
import Hedger.Domain.Snapshot (Snapshot (..))

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

metadataFor :: SnapshotContext -> SnapshotMetadata
metadataFor context =
    SnapshotMetadata
        { revision = context.snapshot.revision
        , freshness = maybe "fresh" (const "stale") context.reloadError
        , warnings =
            maybe
                []
                (const ["The latest journal reload failed; the last usable snapshot is shown."])
                context.reloadError
        }
