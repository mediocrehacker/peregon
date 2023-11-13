module Data.Conversion.Params where

import Data.Aeson (FromJSON, ToJSON, Value)

data Params = Params
  { tex_file :: Text,
    customization :: Maybe (HashMap Text Value)
  }
  deriving stock (Eq, Show,Generic)

instance FromJSON Params
instance ToJSON Params
