module Data.Env where

newtype Env = Env
  { config :: Text
  }

init :: Text -> Env
init = Env
