module Telegram.Query
  ( api,
    valueKeyBoard,
    valueUpdate,
    valueStiker,
    valueMessage,
  )
where

import Data.Aeson
  ( KeyValue ((.=)),
    Value,
    object,
  )
import Data.Text (Text)
import qualified Data.Text as T
import Telegram.Api
  ( InlineKeyboardButton (..),
    InlineKeyboardMarkup (..),
  )
import Telegram.Types (ChatId, Token)
import TextShow (TextShow (showt))

-- базовая сылка телеги
api :: Token -> Text
api token = "https://api.telegram.org/bot" `T.append` token

valueMessage :: ChatId -> Text -> Value
valueMessage chatId text =
  object
    [ "chat_id" .= showt chatId,
      "text" .= text
    ]

valueStiker :: ChatId -> Text -> Value
valueStiker chatId text =
  object
    [ "chat_id" .= showt chatId,
      "sticker" .= text
    ]

valueKeyBoard :: ChatId -> Value
valueKeyBoard chatId =
  object
    [ "chat_id" .= chatId,
      "text" .= ("repeat" :: Text),
      "reply_markup" .= keyboardJSON,
      "one_time_keyboard" .= True
    ]

valueUpdate :: Int -> Value
valueUpdate offset =
  object
    ["offset" .= showt offset]

keyboardJSON :: InlineKeyboardMarkup
keyboardJSON = InlineKeyboardMarkup $ [map cons_num [1, 2, 3, 4, 5]]
  where
    cons_num :: Int -> InlineKeyboardButton
    cons_num (showt -> n) = InlineKeyboardButton n n
