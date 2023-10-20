module Server where

import Data.Aeson
import Data.Text qualified as T
import Data.Env (Env)
import Data.Conversion.Params qualified as CP
import Servant
import Query qualified as Query

type API =
  "api" :> "v1"
    :> ( "healthcheck" :> Get '[JSON] Healthcheck
           :<|> "conversion" :> ReqBody '[JSON] CP.Params:> Post '[JSON] (Response CP.Params)
       )

newtype Healthcheck = Healthcheck
  {status :: Text}
  deriving newtype (Eq, Show)
  deriving stock (Generic)

instance ToJSON Healthcheck

data Response a
  = RespSuccess a
  | RespFailure Text
  deriving stock (Generic, Show)

instance ToJSON (Response CP.Params)
  

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

postConversion :: CP.Params -> AppM (Response CP.Params)
postConversion params = do
  resp <- liftIO $ Query.get $ CP.tex_file params
  case resp of
    Left err ->
      return (RespFailure $ T.pack ("Error: " ++ show err))
    Right tex -> do
      return (RespSuccess (CP.Params "name" tex "class" "customization"))

api :: Proxy API
api = Proxy

runApp :: Env -> Application
runApp env = serve api $ hoistServer api (nt env) server

type AppM =
  ReaderT Env Handler

nt :: Env -> AppM a -> Handler a
nt s x = runReaderT x s

