module Server where

import Data.Aeson
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Env (Env)
import Data.Conversion.Params qualified as CP
import Data.Conversion.Response qualified as CR
import Servant
import Query qualified as Query
import System.Process qualified as Process
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString.Base16 qualified as Base16

type API =
  "api" :> "v1"
    :> ( "healthcheck" :> Get '[JSON] Healthcheck
           :<|> "conversion" :> ReqBody '[JSON] CP.Params:> Post '[JSON] (Response CR.Response)
       )

type StaticAPI = "api" :> "v1" :> "static" :> Raw

newtype Healthcheck = Healthcheck
  {status :: Text}
  deriving newtype (Eq, Show)
  deriving stock (Generic)

instance ToJSON Healthcheck

data Response a
  = RespSuccess a
  | RespFailure Text
  deriving stock (Generic, Show)

instance ToJSON (Response CR.Response)
  

healthcheckUp :: Healthcheck
healthcheckUp =
  Healthcheck "UP"

getHealthcheck :: AppM Healthcheck
getHealthcheck =
  return healthcheckUp

postConversion :: CP.Params -> AppM (Response CR.Response)
postConversion params = do
  texResp <- liftIO $ Query.get $ CP.tex_file params
  case texResp of
    Left err ->
      return (RespFailure $ T.pack ("Error: " ++ show err))
    Right tex -> do
      fileName <- liftIO $ conversion (encodeUtf8 tex)
      return (RespSuccess (CR.Response fileName))


conversion :: ByteString -> IO (Text)
conversion tex = do
  _ <- writeFileBS filePath tex 
  _ <- Process.spawnProcess "pdflatex" ["-output-directory=./static/", filePath]
  return (T.pack fileName)
  where
    hash :: BS.ByteString 
    hash =
      Base16.encode $ SHA256.hash tex

    hash' :: String
    hash' = T.unpack $ decodeUtf8 hash

    fileName :: String
    fileName = 
          hash' ++ ".tex"

    filePath :: String
    filePath = 
          "tmp/" ++ fileName 


api :: Proxy API
api = Proxy 

staticAPI :: Proxy StaticAPI
staticAPI = Proxy

staticApp :: Server Raw
staticApp =
 serveDirectoryWebApp "static" 

server :: ServerT API AppM
server =
  getHealthcheck
    :<|> postConversion
  
allApi :: Proxy (API :<|> StaticAPI)
allApi = Proxy

runApp :: Env -> Application
runApp env' = serve allApi
  (hoistServer api (nt env') server :<|> staticApp)

type AppM =
  ReaderT Env Handler

nt :: Env -> AppM a -> Handler a
nt s x = runReaderT x s

