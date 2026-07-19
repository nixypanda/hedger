module Hedger.Interface.HTTP.API (
    API,
    apiProxy,
    application,
)
where

import Data.Text (Text)
import Servant

import Hedger.Application.AppM (Env)
import Hedger.Interface.HTTP.Accounts (AccountsAPI, accountsServer)
import Hedger.Interface.HTTP.Analytics (AnalyticsAPI, analyticsServer)
import Hedger.Interface.HTTP.System (SystemAPI, systemServer)
import Hedger.Interface.HTTP.Transactions (TransactionsAPI, transactionsServer)

type API =
    "api"
        :> "v1"
        :> Header "X-Request-ID" Text
        :> (SystemAPI :<|> AccountsAPI :<|> TransactionsAPI :<|> AnalyticsAPI)

apiProxy :: Proxy API
apiProxy = Proxy

application :: Env -> Application
application environment = serve apiProxy (server environment)

server :: Env -> Server API
server environment requestId =
    systemServer environment requestId
        :<|> accountsServer environment requestId
        :<|> transactionsServer environment requestId
        :<|> analyticsServer environment requestId
