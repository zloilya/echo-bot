module VK.Api
  ( Ok (..),
    VKResponse (..),
    LongPollServer (..),
    Update (..),
    Object (..),
    Message (..),
    Attachments (..),
    Sticker (..),
    KeyBoard (..),
    Action1 (..),
    Action (..),
    Button (..),
    Command (..),
  )
where

import Common (parseJsonDrop, toJsonDrop)
import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

{-
vk send to us Ok structure
-}
data Ok = Ok
  { ok_ts :: Text,
    ok_updates :: [Update]
  }
  deriving (Show, Generic)

instance FromJSON Ok where
  parseJSON = parseJsonDrop 3

{-
getLongPollServer send to us VKResponse structure
-}
data VKResponse = VKResponse
  { response :: LongPollServer
  }
  deriving (FromJSON, Show, Generic)

{-
we need LongPollServer to send back to the vk
-}
data LongPollServer = LongPollServer
  { long_key :: Text,
    long_server :: Text,
    long_ts :: Text
  }
  deriving (Show, Generic)

instance FromJSON LongPollServer where
  parseJSON = parseJsonDrop 5

{-
if Ok is not a error, then we have list Updates
-}
data Update = Update
  { update_object :: Object
  }
  deriving (Show, Generic)

instance FromJSON Update where
  parseJSON = parseJsonDrop 7

{-
some structure for json
-}
data Object = Object
  { object_message :: Message
  }
  deriving (Show, Generic)

instance FromJSON Object where
  parseJSON = parseJsonDrop 7

{-
main strucure when we communicate with user
-}
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

{-
some structure for json
-}
data Attachments = Attachments
  { atta_sticker :: Sticker
  }
  deriving (Show, Generic)

instance FromJSON Attachments where
  parseJSON = parseJsonDrop 5

{-
we need to know sticker_id to send sticker back
-}
data Sticker = Sticker
  { sticker_id :: Int64
  }
  deriving (FromJSON, Show, Generic)

{-
send buttons to user
-}
data KeyBoard = KeyBoard
  { one_time :: Bool,
    buttons :: [[Action1]]
  }
  deriving (ToJSON, Show, Generic)

{-
some structure for json
-}
data Action1 = Action1
  { action :: Action
  }
  deriving (ToJSON, Show, Generic)

{-
vk send label after user press button
-}
data Action = Action
  { act_type :: Text,
    act_payload :: Button,
    act_label :: Text
  }
  deriving (Show, Generic)

instance ToJSON Action where
  toJSON = toJsonDrop 4

{-
button with name
-}
data Button = Button
  { button :: Text
  }
  deriving (FromJSON, ToJSON, Show, Generic)

{-
vk send to us start command in message_payload
-}
data Command = Command
  { command :: Text
  }
  deriving (FromJSON, Show, Generic)
