module Main where

import Main.Utf8 qualified as Utf8
import Data.Env qualified as Env
import Server (runApp)
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  -- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
  Utf8.withUtf8 $ do
    run 8081 $ runApp (Env.init "NoConfig Yet")
