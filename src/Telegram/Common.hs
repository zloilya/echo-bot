module Telegram.Common where 

import Data.Int (Int64)
import Data.Text (Text)

-- удобна не редачить сигнатуру для функций
data Env = Env
  { env_chat_id :: Int64,
    env_response_text :: Text,
    env_send :: Env -> Text
  }










