module Main where

import Main.Utf8 qualified as Utf8
import Network.Wai.Handler.Warp (run)
import Server (runApp, initEnv)

main :: IO ()
main = do
  -- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
  Utf8.withUtf8 $ do
    run 8081 $ runApp (initEnv "NoConfig Yet")
