module VK.Config where

import Data.Text (Text)
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)

data Config = Config
  { token :: Text,
    groupId :: Text,
    tableString :: String
  }
  deriving (FromJSON, Generic, Eq, Show)
