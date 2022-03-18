module Telegram.Api
  ( Ok (..),
    Update (..),
    Message (..),
    Chat (..),
    Sticker (..),
    InlineKeyboardMarkup (..),
    InlineKeyboardButton (..),
    CallbackQuery (..),
  )
where

import Common (parseJsonDrop, toJsonDrop)
import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import Data.Int (Int64)
import Data.Text (Text)
import Deriving.Aeson (CustomJSON (..), FieldLabelModifier, StripPrefix, SumUntaggedValue)
import GHC.Generics (Generic)
import Telegram.Types (StickerId)

{-
telegram send to us Ok structure
-}
data Ok a = Ok
  { ok :: Bool,
    result :: a
  }
  deriving (FromJSON, Show, Generic)

{-
if Ok True, then we have list Updates
-}
data Update
  = UpdateMessage {- when we get message -}
      { update_id :: Int,
        message :: Message
      }
  | UpdateCallBack {- when we get callback -}
      { update_id :: Int,
        callback_query :: CallbackQuery
      }
  | UpdateUnknown {- when we don't know -}
      { update_id :: Int
      }
  deriving stock (Show, Generic)
  deriving (FromJSON) via (CustomJSON '[SumUntaggedValue] Update)

{-
main strucure when we communicate with user
-}
data Message
  = MessageText {- when we get usuallt text message -}
      { chat :: Chat,
        text :: Text
      }
  | MessageSticker {- when user spam skickers -}
      { chat :: Chat,
        sticker :: Sticker
      }
  | MessageUnknown {- user send unsupported message -}
      { chat :: Chat
      }
  deriving stock (Show, Generic)
  deriving (FromJSON) via (CustomJSON '[SumUntaggedValue] Message)

{-
we need to know chat_id to send back message for user
-}
data Chat = Chat
  { chat_id :: Int64
  }
  deriving (Show, Generic)

instance FromJSON Chat where
  parseJSON = parseJsonDrop 5

{-
we need to know file_id to send sticker back
-}
data Sticker = Sticker
  { file_id :: Text
  }
  deriving (FromJSON, Show, Generic)

{-
send buttons to user
-}
data InlineKeyboardMarkup = InlineKeyboardMarkup
  { inline_keyboard :: [[InlineKeyboardButton]]
  }
  deriving (ToJSON, Show, Generic)

{-
button with name and callback
-}
data InlineKeyboardButton = InlineKeyboardButton
  { ikb_text :: Text,
    ikb_callback_data :: Text
  }
  deriving (Show, Generic)

instance ToJSON InlineKeyboardButton where
  toJSON = toJsonDrop 4

{-
callback when somebody press botton in keybord
-}
data CallbackQuery = CallbackQuery
  { cq_message :: Message,
    cq_data :: Text
  }
  deriving (Show, Generic)

instance FromJSON CallbackQuery where
  parseJSON = parseJsonDrop 3
