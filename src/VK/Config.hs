module VK.Config (Config (..)) where

import Data.Text (Text)
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)

{-
vk config from config.yaml
we need tableString bacause Table (Postgress) is a ByteString and
we don't have FromJSON for it :(
-}
data Config = Config
  { token :: Text,
    groupId :: Text,
    tableString :: String
  }
  deriving (FromJSON, Generic, Eq, Show)
