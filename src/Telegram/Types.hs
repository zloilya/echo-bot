module Telegram.Types where

import Data.Int (Int64)
import Data.Text (Text)
import Database.PostgreSQL.Simple.Types (Query)

type Token = Text

data Env = Env
  { token :: Token,
    offset :: Int,
    path :: FilePath,
    help :: Text,
    repeat :: Text,
    defaultRepeat :: Int,
    table :: Query
  }

type ChatId = Int64

type RawResponse = Text

-- резльтат обработки пользовательских действий
data MessageRequest
  = Stick Text -- для стикеров
  | Help -- для /help
  | Mes Text -- для всех текстовых сообщений не являющимися командами
  | Repeat -- для /repeat
  | Start -- для /start
