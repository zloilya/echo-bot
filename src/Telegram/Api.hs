module Telegram.Api where

import Common (parseJsonDrop, toJsonDrop)
import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

data Ok a = Ok
  { ok :: Bool,
    result :: a
  }
  deriving (FromJSON, Show, Generic)

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
  deriving (FromJSON, ToJSON, Show, Generic)

data InlineKeyboardButton = InlineKeyboardButton
  { ikb_text :: Text,
    ikb_callback_data :: Maybe Text
  }
  deriving (Show, Generic)

instance ToJSON InlineKeyboardButton where
  toJSON = toJsonDrop 4

instance FromJSON InlineKeyboardButton where
  parseJSON = parseJsonDrop 4

data CallbackQuery = CallbackQuery
  { cq_message :: Maybe Message,
    cq_data :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON CallbackQuery where
  parseJSON = parseJsonDrop 3

data Update = Update
  { update_id :: Int,
    message :: Maybe Message,
    callback_query :: Maybe CallbackQuery
  }
  deriving (FromJSON, Show, Generic)

data Message = Message
  { chat :: Chat,
    text :: Maybe Text,
    sticker :: Maybe Sticker
  }
  deriving (FromJSON, Show, Generic)
