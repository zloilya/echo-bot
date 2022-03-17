module VK.Api where

import Common (parseJsonDrop, toJsonDrop)
import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

data Ok = Ok
  { ok_ts :: Text,
    ok_updates :: [Update]
  }
  deriving (Show, Generic)

instance FromJSON Ok where
  parseJSON = parseJsonDrop 3

data VKResponse = VKResponse
  { response :: LongPollServer
  }
  deriving (FromJSON, Show, Generic)

data LongPollServer = LongPollServer
  { long_key :: Text,
    long_server :: Text,
    long_ts :: Text
  }
  deriving (Show, Generic)

instance FromJSON LongPollServer where
  parseJSON = parseJsonDrop 5

data Update = Update
  { update_object :: Object
  }
  deriving (Show, Generic)

instance FromJSON Update where
  parseJSON = parseJsonDrop 7

data Object = Object
  { object_message :: Message
  }
  deriving (Show, Generic)

instance FromJSON Object where
  parseJSON = parseJsonDrop 7

data Message = Message
  { message_from_id :: Int64,
    message_attachments :: [Attachments],
    message_peer_id :: Int64,
    message_text :: Text,
    message_payload :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Message where
  parseJSON = parseJsonDrop 8

data Attachments = Attachments
  { atta_sticker :: Sticker
  }
  deriving (Show, Generic)

instance FromJSON Attachments where
  parseJSON = parseJsonDrop 5

data Sticker = Sticker
  { sticker_id :: Int64
  }
  deriving (FromJSON, Show, Generic)

data KeyBoard = KeyBoard
  { one_time :: Bool,
    buttons :: [[Action1]]
  }
  deriving (ToJSON, Show, Generic)

data Action1 = Action1
  { action :: Action
  }
  deriving (ToJSON, Show, Generic)

data Action = Action
  { act_type :: Text,
    act_payload :: Button,
    act_label :: Text
  }
  deriving (Show, Generic)

instance ToJSON Action where
  toJSON = toJsonDrop 4

data Button = Button
  { button :: Text
  }
  deriving (FromJSON, ToJSON, Show, Generic)

data Command = Command
  { command :: Text
  }
  deriving (FromJSON, Show, Generic)
