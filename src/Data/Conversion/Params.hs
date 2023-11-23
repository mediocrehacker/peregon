module Data.Conversion.Params where

import Data.Aeson (FromJSON, ToJSON)

data Params = Params
  { cls :: Maybe Text
  , tex :: Text
  , customization :: Maybe (HashMap Text Text)
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON Params
instance ToJSON Params
