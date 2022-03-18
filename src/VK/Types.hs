module VK.Types
  ( Env (..),
    MessageRequest (..),
    Token,
    GroupId,
    UserId,
    PeerId,
    StikerId,
    RandomId,
    ChatId,
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import PostgresQuery (Table)
import VK.Api (LongPollServer)

{-
environment builds from vk config and default settings
-}
data Env = Env
  { token :: Token,
    groupId :: GroupId,
    lps :: LongPollServer,
    help :: Text,
    repeat :: Text,
    defaultRepeat :: Int,
    table :: Table
  }

{-
result processing message
-}
data MessageRequest
  = Help -- for /help
  | Repeat -- for /repeat
  | Mes Text -- for text

type Token = Text

type GroupId = Text

type UserId = Int64

type PeerId = Int64

type StikerId = Int64

type RandomId = Int64

type ChatId = Int64