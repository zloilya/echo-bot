module VK.Types where

import Data.Int (Int64)
import Data.Text (Text)
import PostgresQuery (Table)
import VK.Api (LongPollServer)

type Token = Text

type GroupId = Text

data Env = Env
  { token :: Token,
    groupId :: GroupId,
    lps :: LongPollServer,
    help :: Text,
    repeat :: Text,
    defaultRepeat :: Int,
    table :: Table
  }

type UserId = Int64

type PeerId = Int64

type StikerId = Int64

type RandomId = Int64

type ChatId = Int64