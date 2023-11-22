module Server where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.Conversion.Params qualified as CP
import Data.Conversion.Response qualified as CR
import Data.Documentclass.Params qualified as DP
import Data.Documentclass.Response qualified as DR
import Data.Env qualified as Env
import Data.Text qualified as T
import Query qualified as Query
import Relude.Extra.Map (toPairs)
import Servant
import System.Process qualified as Process

type API =
  "api" :> "v1"
    :> ( "healthcheck" :> Get '[JSON] Healthcheck
           :<|> "conversion" :> ReqBody '[JSON] CP.Params :> Post '[JSON] (Response CR.Response)
           :<|> "documentclass" :> ReqBody '[JSON] DP.Params :> Post '[JSON] (Response DR.Response)
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

instance ToJSON (Response DR.Response)

healthcheckUp :: Healthcheck
healthcheckUp =
  Healthcheck "UP"

getHealthcheck :: AppM Healthcheck
getHealthcheck =
  return healthcheckUp

postDocumentclass :: DP.Params -> AppM (Response DR.Response)
postDocumentclass params = do
  texResp <- liftIO $ Query.get $ DP.file params
  case texResp of
    Left err ->
      return $ RespFailure $ T.pack $ "Error: " ++ show err
    Right tex -> do
      fileName <- liftIO $ storeCls $ encodeUtf8 tex
      return $ RespSuccess $ DR.Response fileName

storeCls :: ByteString -> IO (Text)
storeCls tex = do
  _ <- writeFileBS ("static/" ++ T.unpack fileName) tex
  return fileName
  where
    fileName =
      T.append (hash tex) ".cls"

hash :: ByteString -> Text
hash =
  decodeUtf8 . Base16.encode . SHA256.hash

postConversion :: CP.Params -> AppM (Response CR.Response)
postConversion params = do
  texResp <- liftIO $ Query.get $ CP.tex params
  case texResp of
    Left err ->
      return (RespFailure $ T.pack ("Error: " ++ show err))
    Right tex -> do
      fileName <- liftIO $ conversion tex params
      return (RespSuccess (CR.Response fileName))

conversion :: Text -> CP.Params -> IO (Text)
conversion tex params =
  pdfLatex $ encodeUtf8 $ conversionTex tex (CP.cls params) (CP.customization params)

conversionTex :: Text -> Maybe Text -> Maybe (HashMap Text Text) -> Text
conversionTex tex cls =
  customization tex_
  where
    tex_ = maybe tex (addDocumentclass tex) cls

addDocumentclass :: Text -> Text -> Text
addDocumentclass tex cls =
  T.replace "\\documentclass{worksheet}" (T.append (T.append "\\documentclass{" cls) "}") tex

customization :: Text -> Maybe (HashMap Text Text) -> Text
customization tex =
  foldr convert tex . fromMaybe [] . fmap toPairs

pdfLatex :: ByteString -> IO (Text)
pdfLatex tex = do
  _ <- writeFileBS filePath tex
  _ <- Process.spawnProcess "pdflatex" ["-output-directory=./static/", filePath]
  -- xelatex -file-line-error -interaction=nonstopmode -output-directory=./static/ tmp/63bfce1046dc7c8a3f3a5cdba1a5997514eb2493356e1b8a7d88be8daf49c581.tex
  -- pdflatex -file-line-error -interaction=nonstopmode -output-directory=./static/ tmp/63bfce1046dc7c8a3f3a5cdba1a5997514eb2493356e1b8a7d88be8daf49c581.tex
  return fileName
  where
    fileName =
      T.append (hash tex) ".tex"

    filePath =
      "tmp/" ++ T.unpack fileName

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
    :<|> postDocumentclass

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
