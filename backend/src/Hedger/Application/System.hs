module Hedger.Application.System (
    BootstrapInfo (..),
    bootstrap,
    liveness,
    readiness,
)
where

import Control.Monad.Reader (asks)
import Data.Text (Text)

import Hedger.Application.AppM (AppM, Env (..), requireSnapshot)
import Hedger.Application.Common (SnapshotMetadata, metadataFor)
import Hedger.Config (Config (..))

data BootstrapInfo = BootstrapInfo
    { apiVersion :: Text
    , appName :: Text
    , fiscalYearStartMonth :: Int
    , metadata :: SnapshotMetadata
    }

liveness :: AppM ()
liveness = pure ()

readiness :: AppM ()
readiness = requireSnapshot >> pure ()

bootstrap :: AppM BootstrapInfo
bootstrap = do
    snapshot <- requireSnapshot
    fiscal <- asks (.config.fiscalYearStartMonth)
    responseMetadata <- metadataFor snapshot
    pure
        BootstrapInfo
            { apiVersion = "v1"
            , appName = "Hedger"
            , fiscalYearStartMonth = fiscal
            , metadata = responseMetadata
            }
