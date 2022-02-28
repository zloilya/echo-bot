module Telegram.Api where

import Data.Aeson.Types
  ( FromJSON (parseJSON),
    GFromJSON,
    GToJSON,
    Options (..),
    Parser,
    ToJSON (toJSON),
    Value,
    Zero,
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import Data.Int (Int64)
import Data.List (drop, isPrefixOf)
import Data.Text (Text)
import GHC.Generics (Generic (Rep))

-- | Method used to drop prefix from field name during serialization
toJsonDrop :: forall a. (Generic a, GToJSON Zero (Rep a)) => Int -> a -> Value
toJsonDrop prefix =
  genericToJSON
    defaultOptions
      { fieldLabelModifier = drop prefix,
        omitNothingFields = True
      }

-- | Method used to drop prefix from field name during deserialization
parseJsonDrop :: forall a. (Generic a, GFromJSON Zero (Rep a)) => Int -> Value -> Parser a
parseJsonDrop prefix = genericParseJSON defaultOptions {fieldLabelModifier = drop prefix}

data Ok a = Ok
  { ok :: Bool,
    result :: a
  }
  deriving (FromJSON, Show, Generic)

data Chat = Chat
  { chat_id :: Int64
  }
  deriving (Show, Generic)

instance ToJSON Chat where
  toJSON = toJsonDrop 5

instance FromJSON Chat where
  parseJSON = parseJsonDrop 5

data Sticker = Sticker
  { sticker_file_id :: Text
  }
  deriving (Show, Generic)

instance ToJSON Sticker where
  toJSON = toJsonDrop 8

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

instance ToJSON CallbackQuery where
  toJSON = toJsonDrop 3

instance FromJSON CallbackQuery where
  parseJSON = parseJsonDrop 3

data Update = Update
  { update_id :: Int,
    message :: Maybe Message,
    callback_query :: Maybe CallbackQuery
  }
  deriving (FromJSON, ToJSON, Show, Generic)


data Message = Message
  { chat :: Chat,
    text :: Maybe Text,
    sticker :: Maybe Sticker
  }
  deriving (FromJSON, ToJSON, Show, Generic)

