module Config where

import Data.Text (Text)
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)
import qualified Telegram.Config as TG
import qualified VK.Config as VK

{-
описание типа конфига
-}

data Config = Config
  { tgStart :: Bool,
    vkStart :: Bool,
    tgConfig :: TG.Config,
    vkConfig :: VK.Config,
    settings :: Settings
  }
  deriving (FromJSON, Generic, Eq, Show)

data Settings = Settings
  { help :: Text,
    repeat :: Text,
    defaultRepeat :: Int
  }
  deriving (FromJSON, Generic, Eq, Show)
