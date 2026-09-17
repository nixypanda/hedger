module Hedger.Interface.HTTP.API (
    API,
    apiProxy,
    application,
) where

import Data.Text (Text)
import Servant

import Hedger.Application.AppM (Env)
import Hedger.Interface.HTTP.Endpoints.Accounts (AccountsAPI, accountsServer)
import Hedger.Interface.HTTP.Endpoints.Analytics (AnalyticsAPI, analyticsServer)
import Hedger.Interface.HTTP.Endpoints.Events (EventsAPI, eventsServer)
import Hedger.Interface.HTTP.Endpoints.Investments (InvestmentsAPI, investmentsServer)
import Hedger.Interface.HTTP.Endpoints.System (SystemAPI, systemServer)
import Hedger.Interface.HTTP.Endpoints.Transactions (TransactionsAPI, transactionsServer)

type API =
    "api"
        :> "v1"
        :> Header "X-Request-ID" Text
        :> ( SystemAPI
                :<|> AccountsAPI
                :<|> TransactionsAPI
                :<|> AnalyticsAPI
                :<|> InvestmentsAPI
                :<|> EventsAPI
           )

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
        :<|> investmentsServer environment requestId
        :<|> eventsServer environment requestId
