module Hedger.Application.Common (
    SnapshotMetadata (..),
    metadataFor,
    currentMetadata,
    AccountTypeClassifier,
    accountTypeClassifier,
)
where

import Data.Text (Text)

import Hledger.Data.AccountName qualified as HledgerAccountName
import Hledger.Data.Types qualified as Hledger

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)

import Hedger.Application.AppM (AppM, Env (..), SnapshotContext (..))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.DerivedRevision (readDerivedRevision)

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

{- | @derivedRevision@ is the revision the cached analytics data was built from,
@""@ before the first rebuild completes. A reload error outranks a trailing
derived view, because a stale snapshot is the more consequential fact.
-}
metadataFor :: SnapshotContext -> Text -> SnapshotMetadata
metadataFor context derivedRevision =
    SnapshotMetadata
        { revision = context.snapshot.revision
        , freshness =
            if
                | Just _ <- context.reloadError -> "stale"
                | derivedRevision /= context.snapshot.revision -> "refreshing"
                | otherwise -> "fresh"
        , warnings =
            maybe
                []
                (const ["The latest journal reload failed; the last usable snapshot is shown."])
                context.reloadError
        }

{- | 'metadataFor' with the derived revision read from the environment. Every
use case needs it, and reading it per response is what makes @refreshing@
observable while a rebuild trails the published snapshot.
-}
currentMetadata :: SnapshotContext -> AppM SnapshotMetadata
currentMetadata context = do
    derived <- asks (.derivedState)
    revision <- liftIO (readDerivedRevision derived)
    pure (metadataFor context revision)
