module Server where

import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson 
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Conversion.Params as CP
import qualified Data.Conversion.Response as CR
import qualified Data.Env as Env
import qualified Data.Text as T
import qualified Query as Query
import Relude.Extra.Map (toPairs)
import Servant
import qualified System.Process as Process

type API =
  "api" :> "v1"
    :> ( "healthcheck" :> Get '[JSON] Healthcheck
           :<|> "conversion" :> ReqBody '[JSON] CP.Params :> Post '[JSON] (Response CR.Response)
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
      fileName <- liftIO $ conversion tex $ CP.customization params
      return (RespSuccess (CR.Response fileName))

conversion :: Text -> Maybe (HashMap Text Text) -> IO (Text)
conversion tex =
  pdfLatex . encodeUtf8 . foldr convert tex . fromMaybe [] . fmap toPairs

pdfLatex :: ByteString-> IO (Text)
pdfLatex tex = do
  _ <- writeFileBS filePath tex
  _ <- Process.spawnProcess "pdflatex" ["-output-directory=./static/", filePath]
  return (T.pack fileName)
  where
    hash :: BS.ByteString
    hash =
      Base16.encode $ SHA256.hash tex

    fileName :: String
    fileName =
      (T.unpack $ decodeUtf8 hash) ++ ".tex"

    filePath :: String
    filePath =
      "tmp/" ++ fileName

convert :: (Text, Text) -> Text -> Text
convert (key, value) =
  T.replace key_ value
  where
    key_ = T.append (T.append "((" key) "))"

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

initEnv :: Text -> Env.Env
initEnv =
  Env.init

runApp :: Env.Env -> Application
runApp env' =
  serve
    allApi
    (hoistServer api (nt env') server :<|> staticApp)

type AppM =
  ReaderT Env.Env Handler

nt :: Env.Env -> AppM a -> Handler a
nt s x = runReaderT x s
