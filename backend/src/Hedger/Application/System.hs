module Hedger.Application.System (
    BootstrapInfo (..),
    bootstrap,
    liveness,
    readiness,
) where

import Control.Monad.Reader (asks)
import Data.Text (Text)

import Hedger.Application.AppM (AppM, Env (..), requireSnapshot)
import Hedger.Application.Common (SnapshotMetadata, metadataFor)
import Hedger.Config (Config (..))
import Hedger.Domain.Period (monthOfYearValue)

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
    snapshotContext <- requireSnapshot
    fiscal <- asks (.config.fiscalYearStartMonth)
    let responseMetadata = metadataFor snapshotContext
    pure
        BootstrapInfo
            { apiVersion = "v1"
            , appName = "Hedger"
            , fiscalYearStartMonth = monthOfYearValue fiscal
            , metadata = responseMetadata
            }
