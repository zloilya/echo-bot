module Config where

import Data.Text (Text)
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)
import qualified Telegram.Config as TG
import qualified VK.Config as VK

{-
this type represent yaml config
-}
data Config = Config
  { tgStart :: Bool,
    vkStart :: Bool,
    tgConfig :: TG.Config,
    vkConfig :: VK.Config,
    settings :: Settings
  }
  deriving (FromJSON, Generic, Eq, Show)

{-
defualt settigs for task
-}
data Settings = Settings
  { help :: Text,
    repeat :: Text,
    defaultRepeat :: Int,
    loglevel :: Priority,
    database :: Database
  }
  deriving (FromJSON, Generic, Eq, Show)

{-
log levels
-}
data Priority
  = -- | Debug messages
    Debug
  | -- | Notable information that requires no immediate action.
    Info
  | -- | Something is probably wrong, and we should investigate.
    Warn
  deriving (FromJSON, Generic, Eq, Ord, Show)

{-
database settings
-}
data Database = Database
  { host :: Text,
    dbname :: Text
  }
  deriving (FromJSON, Generic, Eq, Ord, Show)
