module VK.Types where

import Data.Int (Int64)
import Data.Text (Text)
import VK.Api (LongPollServer)

type Token = Text

type GroupId = Text

data Env = Env
  { token :: Token,
    groupId :: GroupId,
    lps :: LongPollServer
  }

type UserId = Int64

type PeerId = Int64

type StikerId = Int64

type RandomId = Int64