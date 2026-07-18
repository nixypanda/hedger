module Hedger.Interface.HTTP.System (
    SystemAPI,
    systemServer,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant

import Hedger.Application.AppM (Env)
import Hedger.Application.System (BootstrapInfo (..), bootstrap, liveness, readiness)
import Hedger.Interface.HTTP.Common (ResponseMetadata (..), runAction, toResponseMetadata)

-- DTO declaration

data Health = Health
    { status :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Bootstrap = Bootstrap
    { apiVersion :: Text
    , appName :: Text
    , fiscalYearStartMonth :: Int
    , metadata :: ResponseMetadata
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- API and server

type SystemAPI =
    "health" :> "live" :> Get '[JSON] Health
        :<|> "health" :> "ready" :> Get '[JSON] Health
        :<|> "bootstrap" :> Get '[JSON] Bootstrap

systemServer :: Env -> Maybe Text -> Server SystemAPI
systemServer env requestId =
    runAction env requestId (Health{status = "ok"} <$ liveness)
        :<|> runAction env requestId (Health{status = "ok"} <$ readiness)
        :<|> runAction env requestId (toBootstrap <$> bootstrap)

-- Mapper: domain <-> DTO

toBootstrap :: BootstrapInfo -> Bootstrap
toBootstrap value =
    Bootstrap
        { apiVersion = value.apiVersion
        , appName = value.appName
        , fiscalYearStartMonth = value.fiscalYearStartMonth
        , metadata = toResponseMetadata value.metadata
        }
