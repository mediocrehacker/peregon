module Server where

import Data.Aeson
import Data.Env (Env)
import Servant


type API =
  "api" :> "v1"
    :> ( "healthcheck" :> Get '[JSON] Healthcheck
    -- :<|> ReqBody '[JSON] ConversionReq :> Post '[JSON] ConversionReq
       )

newtype ConversionReq = ConversionReq
  {name :: Text}
  deriving newtype (Eq, Show)
  deriving stock (Generic)

newtype Healthcheck = Healthcheck
  {status :: Text}
  deriving newtype (Eq, Show)
  deriving stock (Generic)

instance ToJSON Healthcheck

healthcheckUp :: Healthcheck
healthcheckUp =
  Healthcheck "UP"

server :: ServerT API AppM
server = return healthcheckUp

api :: Proxy API
api = Proxy

runApp :: Env -> Application
runApp env = serve api $ hoistServer api (nt env) server


type AppM =
  ReaderT Env Handler

nt :: Env -> AppM a -> Handler a
nt s x = runReaderT x s
