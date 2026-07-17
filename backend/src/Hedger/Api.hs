module Hedger.Api (
    API,
    apiProxy,
    application,
)
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Hedger.Application (AppM, Env, bootstrap, liveness, readiness, runAppM, transactionList)
import Hedger.Domain.Error (AppError, appErrorStatus, toEnvelope)
import Hedger.Domain.Types (Bootstrap, Health, TransactionsResponse)
import Network.HTTP.Types qualified as HTTP
import Servant

type API =
    "api"
        :> "v1"
        :> Header "X-Request-ID" Text
        :> ( "health" :> "live" :> Get '[JSON] Health
                :<|> "health" :> "ready" :> Get '[JSON] Health
                :<|> "bootstrap" :> Get '[JSON] Bootstrap
                :<|> "transactions" :> Get '[JSON] TransactionsResponse
           )

apiProxy :: Proxy API
apiProxy = Proxy

application :: Env -> Application
application environment = serve apiProxy (server environment)

server :: Env -> Server API
server environment requestId =
    runAction liveness
        :<|> runAction readiness
        :<|> runAction bootstrap
        :<|> runAction transactionList
  where
    effectiveRequestId = maybe "request-id-missing" id requestId
    runAction :: AppM value -> Handler value
    runAction action = do
        result <- liftIO (runAppM environment action)
        either (throwError . servantError effectiveRequestId) pure result

servantError :: Text -> AppError -> ServerError
servantError requestId appError =
    ServerError
        { errHTTPCode = statusCode
        , errReasonPhrase = "Hedger API error"
        , errBody = Aeson.encode (toEnvelope requestId appError)
        , errHeaders = [(HTTP.hContentType, "application/json; charset=utf-8")]
        }
  where
    statusCode = HTTP.statusCode (appErrorStatus appError)
