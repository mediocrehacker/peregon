module Data.Documentclass.Params where

import Data.Aeson (FromJSON, ToJSON)

newtype Params = Params
  { file :: Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON Params
instance ToJSON Params
