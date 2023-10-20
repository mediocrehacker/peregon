module Query where

import Data.Tex
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client

get :: Text -> IO (Either ClientError Tex)
get url = do
  manager' <- newManager tlsManagerSettings
  burl <- parseBaseUrl (toString url)
  res <- runClientM queries (mkClientEnv manager' burl)
  return res

type API = Get '[PlainText] Tex

api :: Proxy API
api = Proxy

tex :: ClientM Tex
tex = client api

queries :: ClientM (Tex)
queries = do
  tex' <- tex
  return (tex')
