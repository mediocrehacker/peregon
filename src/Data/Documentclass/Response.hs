module Data.Documentclass.Response where

import Data.Aeson (FromJSON, ToJSON)

data Response = Response 
  { fileName :: Text
  }
  deriving stock (Eq, Show,Generic)

instance FromJSON Response 

instance ToJSON Response 
