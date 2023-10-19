module Query where

import Data.Tex
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Servant.Client.Internal.HttpClient as I
import Servant.API
import Servant.Client

get :: Text -> IO (Either ClientError Tex)
get url = do
  -- manager' <- newManager defaultManagerSettings
  manager' <- newManager tlsManagerSettings
  burl <- parseBaseUrl (toString url)
  -- mgr <- newManager defaultManagerSettings
  -- let req' = I.requestToClientRequest burl req
  res <- runClientM queries (mkClientEnv manager' burl)
  putStrLn $ "Making request: " ++ show res
  return res

type API = "worksheet.tex" :> Get '[PlainText] Tex

api :: Proxy API
api = Proxy

tex :: ClientM Tex
tex = client api

queries :: ClientM (Tex)
queries = do
  tex' <- tex
  return (tex')
