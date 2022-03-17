module Telegram.Types
  ( Env (..),
    MessageRequest (..),
    ChatId,
    Table,
    Token,
    StickerId,
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import PostgresQuery (Table)

type Token = Text

type StickerId = Text

{-
envaroment builds from telegram config and default settings
-}
data Env = Env
  { token :: Token,
    offset :: Int,
    path :: FilePath,
    help :: Text,
    repeat :: Text,
    defaultRepeat :: Int,
    table :: Table
  }

type ChatId = Int64

{-
result processing message
-}
data MessageRequest
  = Stick Text -- for sticker
  | Help -- for /help
  | Mes Text -- for text
  | Repeat -- for /repeat
  | Start -- for /start
