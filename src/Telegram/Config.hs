module Telegram.Config (Config (..)) where

import Data.Text (Text)
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)

{-
telegram config from config.yaml
we need tableString bacause Table (Postgress) is a ByteString and
we don't have FromJSON for it :(
-}
data Config = Config
  { token :: Text,
    path :: FilePath,
    tableString :: String
  }
  deriving (FromJSON, Generic, Eq, Show)
