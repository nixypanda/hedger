module Hedger.Application.Common (
    SnapshotMetadata (..),
    metadataFor,
)
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (Text)

import Hedger.Application.AppM (AppM, Env (..))
import Hedger.Domain.Snapshot (Snapshot (..))
import Hedger.Infrastructure.SnapshotStore (readLastReloadError)

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
