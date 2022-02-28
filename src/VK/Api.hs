module VK.Api where

import Data.Aeson.Types
  ( FromJSON (parseJSON),
    GFromJSON,
    GToJSON,
    Options (..),
    Parser,
    SumEncoding (..),
    ToJSON (toJSON),
    Value (String),
    Zero,
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import qualified Data.Char as Char
import Data.Int (Int32, Int64)
import Data.List (drop, isPrefixOf)
import Data.Text (Text, unpack)
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
  { update_type :: Text,
    update_object :: Object,
    update_group_id :: Int64
  }
  deriving (Show, Generic)

instance FromJSON Update where
  parseJSON = parseJsonDrop 7

data Object = Object
  { object_message :: Message,
    object_client_info :: ClientInfo
  }
  deriving (Show, Generic)

instance FromJSON Object where
  parseJSON = parseJsonDrop 7

data Message = Message
  { message_date :: Int64,
    message_from_id :: Int64,
    message_id :: Int64,
    message_out :: Int64,
    message_attachments :: [Attachments],
    message_conversation_message_id :: Int64,
    message_fwd_messages :: [Message],
    message_important :: Bool,
    message_is_hidden :: Bool,
    message_payload :: Maybe Text,
    message_peer_id :: Int64,
    message_random_id :: Int64,
    message_text :: Text
  }
  deriving (Show, Generic)

instance FromJSON Message where
  parseJSON = parseJsonDrop 8

data ClientInfo = ClientInfo
  { button_actions :: [Text],
    keyboard :: Bool,
    inline_keyboard :: Bool,
    carousel :: Bool,
    lang_id :: Int64
  }
  deriving (FromJSON, Show, Generic)

data Attachments = Attachments
  { atta_type :: Text,
    atta_sticker :: Sticker
  }
  deriving (Show, Generic)

instance FromJSON Attachments where
  parseJSON = parseJsonDrop 5

data Sticker = Sticker
  { product_id :: Int64,
    sticker_id :: Int64,
    images :: [Image],
    images_with_background :: [Image],
    animation_url :: Maybe Text,
    is_allowed :: Maybe Bool
  }
  deriving (FromJSON, Show, Generic)

data Image = Image
  { url :: Text,
    width :: Int64,
    height :: Int64
  }
  deriving (FromJSON, Show, Generic)
