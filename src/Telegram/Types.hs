module Telegram.Types
  ( Env (..),
    MessageRequest (..),
    ChatId,
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
environment builds from telegram config and default settings
-}
data Env = Env
  { token :: Token,
    offset :: Int,
    path :: FilePath,
    help :: Text,
    repeat :: Text,
    defaultRepeat :: Int,
    table :: Table,
    logDebug :: String -> IO (),
    logInfo :: String -> IO (),
    logWarn :: String -> IO (),
    host :: Text,
    dbname :: Text
  }

type ChatId = Int64

{-
result processing message
-}
data MessageRequest
  = Start -- for /start
  | Help -- for /help
  | Repeat -- for /repeat
  | Stick Text -- for sticker
  | Mes Text -- for text
