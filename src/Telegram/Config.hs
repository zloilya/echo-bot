module Telegram.Config where

import Data.Text (Text)
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)

data Config = Config
  { token :: Text,
    path :: FilePath,
    tableString :: String
  }
  deriving (FromJSON, Generic, Eq, Show)
