module Server where

import Data.Aeson
import Data.Env (Env)
import Data.Conversion.Params qualified as CP
import Servant

type API =
  "api" :> "v1"
    :> ( "healthcheck" :> Get '[JSON] Healthcheck
           :<|> "conversion" :> ReqBody '[JSON] CP.Params:> Post '[JSON] CP.Params 
       )

newtype Healthcheck = Healthcheck
  {status :: Text}
  deriving newtype (Eq, Show)
  deriving stock (Generic)

instance ToJSON Healthcheck

healthcheckUp :: Healthcheck
healthcheckUp =
  Healthcheck "UP"

server :: ServerT API AppM
server =
  getHealthcheck
    :<|> postConversion

getHealthcheck :: AppM Healthcheck
getHealthcheck =
  return healthcheckUp

postConversion :: CP.Params -> AppM CP.Params
postConversion _ =
  return (CP.Params "name" "tex" "class" "customization")

api :: Proxy API
api = Proxy

runApp :: Env -> Application
runApp env = serve api $ hoistServer api (nt env) server

type AppM =
  ReaderT Env Handler

nt :: Env -> AppM a -> Handler a
nt s x = runReaderT x s
