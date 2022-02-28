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
    message_text :: Text
  }
  deriving (Show, Generic)

instance FromJSON Message where
  parseJSON = parseJsonDrop 8

data Attachments = Attachments
  { atta_type :: Text,
    atta_sticker :: Sticker
  }
  deriving (Show, Generic)

instance FromJSON Attachments where
  parseJSON = parseJsonDrop 5

data Sticker = Sticker
  { sticker_id :: Int64
  }
  deriving (FromJSON, Show, Generic)
