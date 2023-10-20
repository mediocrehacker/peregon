module Data.Conversion.Params where

import Data.Aeson (FromJSON, ToJSON)

data UsersError = UserNotFound

data Params = Params
  { name :: Text,
    tex_file :: Text,
    class_file :: Text,
    customization :: Text
  }
  deriving stock (Eq, Show,Generic)

instance FromJSON Params

instance ToJSON Params
