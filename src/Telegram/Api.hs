{-# LANGUAGE DerivingVia #-}

module Telegram.Api where

import Common (parseJsonDrop, toJsonDrop)
import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import Data.Int (Int64)
import Data.Text (Text)
import Deriving.Aeson (CustomJSON (..), FieldLabelModifier, StripPrefix, SumUntaggedValue)
import GHC.Generics (Generic)

data Ok a = Ok
  { ok :: Bool,
    result :: a
  }
  deriving (FromJSON, Show, Generic)

data Update
  = UpdateMessage
      { update_id :: Int,
        message :: Message
      }
  | UpdateCallBack
      { update_id :: Int,
        callback_query :: CallbackQuery
      }
  | UpdateUnknown
      { update_id :: Int
      }
  deriving stock (Show, Generic)
  deriving (FromJSON) via (CustomJSON '[SumUntaggedValue] Update)

data Message
  = MessageText
      { chat :: Chat,
        text :: Text
      }
  | MessageSticker
      { chat :: Chat,
        sticker :: Sticker
      }
  | MessageUnknown
      { chat :: Chat
      }
  deriving stock (Show, Generic)
  deriving (FromJSON) via (CustomJSON '[SumUntaggedValue] Message)

data Chat = Chat
  { chat_id :: Int64
  }
  deriving (Show, Generic)

instance FromJSON Chat where
  parseJSON = parseJsonDrop 5

data Sticker = Sticker
  { sticker_file_id :: Text
  }
  deriving (Show, Generic)

instance FromJSON Sticker where
  parseJSON = parseJsonDrop 8

data InlineKeyboardMarkup = InlineKeyboardMarkup
  { inline_keyboard :: [[InlineKeyboardButton]]
  }
  deriving (ToJSON, Show, Generic)

data InlineKeyboardButton = InlineKeyboardButton
  { ikb_text :: Text,
    ikb_callback_data :: Text
  }
  deriving (Show, Generic)

instance ToJSON InlineKeyboardButton where
  toJSON = toJsonDrop 4

data CallbackQuery = CallbackQuery
  { cq_message :: Message,
    cq_data :: Text
  }
  deriving (Show, Generic)

instance FromJSON CallbackQuery where
  parseJSON = parseJsonDrop 3
