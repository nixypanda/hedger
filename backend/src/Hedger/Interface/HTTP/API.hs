module Hedger.Interface.HTTP.API (
    API,
    apiProxy,
    application,
)
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Hedger.Application (
    AppM,
    BootstrapInfo (..),
    Env,
    SnapshotMetadata (..),
    TransactionList (..),
    bootstrap,
    liveness,
    readiness,
    runAppM,
    transactionList,
 )
import Hedger.Domain.Error (AppError)
import Hedger.Interface.HTTP.Errors (appErrorStatus, toEnvelope)
import Hedger.Interface.HTTP.TransactionMapper (toTransactions)
import Hedger.Interface.HTTP.Types (
    Bootstrap (..),
    Health (..),
    ResponseMetadata (..),
    TransactionsResponse (..),
 )
import Network.HTTP.Types qualified as HTTP
import Servant

type API =
    "api"
        :> "v1"
        :> Header "X-Request-ID" Text
        :> ( "health" :> "live" :> Get '[JSON] Health
                :<|> "health" :> "ready" :> Get '[JSON] Health
                :<|> "bootstrap" :> Get '[JSON] Bootstrap
                :<|> "transactions"
                    :> QueryParam "query" Text
                    :> QueryParam "limit" Text
                    :> QueryParam "cursor" Text
                    :> Get '[JSON] TransactionsResponse
           )

apiProxy :: Proxy API
apiProxy = Proxy

application :: Env -> Application
application environment = serve apiProxy (server environment)

server :: Env -> Server API
server environment requestId =
    runAction (Health{status = "ok"} <$ liveness)
        :<|> runAction (Health{status = "ok"} <$ readiness)
        :<|> runAction (toBootstrap <$> bootstrap)
        :<|> \query limit cursor ->
            runAction (toTransactionsResponse <$> transactionList query limit cursor)
  where
    effectiveRequestId = maybe "request-id-missing" id requestId
    runAction :: AppM value -> Handler value
    runAction action = do
        result <- liftIO (runAppM environment action)
        either (throwError . servantError effectiveRequestId) pure result

servantError :: Text -> AppError -> ServerError
servantError requestId appError =
    ServerError
        { errHTTPCode = HTTP.statusCode (appErrorStatus appError)
        , errReasonPhrase = "Hedger API error"
        , errBody = Aeson.encode (toEnvelope requestId appError)
        , errHeaders = [(HTTP.hContentType, "application/json; charset=utf-8")]
        }

toBootstrap :: BootstrapInfo -> Bootstrap
toBootstrap value =
    Bootstrap
        { apiVersion = value.apiVersion
        , appName = value.appName
        , fiscalYearStartMonth = value.fiscalYearStartMonth
        , defaultValuationBasis = value.defaultValuationBasis
        , metadata = toResponseMetadata value.metadata
        }

toTransactionsResponse :: TransactionList -> TransactionsResponse
toTransactionsResponse value =
    TransactionsResponse
        { metadata = toResponseMetadata value.metadata
        , transactions = toTransactions value.transactions
        , nextCursor = value.nextCursor
        }

toResponseMetadata :: SnapshotMetadata -> ResponseMetadata
toResponseMetadata value =
    ResponseMetadata
        { revision = value.revision
        , freshness = value.freshness
        , warnings = value.warnings
        }
