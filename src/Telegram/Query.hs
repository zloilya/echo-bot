module Telegram.Query
  ( valueKeyBoard,
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
import Telegram.Types (ChatId, StickerId, Token)
import TextShow (TextShow (showt))

{-
we need to know chat_id and text to send message for user
-}
valueMessage :: ChatId -> Text -> Value
valueMessage chatId text =
  object
    [ "chat_id" .= showt chatId,
      "text" .= text
    ]

{-
we need to know chat_id and sticker to send message with sticker for user
-}
valueStiker :: ChatId -> StickerId -> Value
valueStiker chatId stickerId =
  object
    [ "chat_id" .= showt chatId,
      "sticker" .= stickerId
    ]

{-
we need to know chat_id to send keybiar for user
-}
valueKeyBoard :: ChatId -> Value
valueKeyBoard chatId =
  object
    [ "chat_id" .= chatId,
      "text" .= ("repeat" :: Text),
      "reply_markup" .= keyboard,
      "one_time_keyboard" .= True
    ]

{-
maybe we need offset to get update
-}
valueUpdate :: Int -> Value
valueUpdate offset =
  object
    ["offset" .= showt offset]

{-
it is a keyboard
-}
keyboard :: InlineKeyboardMarkup
keyboard = InlineKeyboardMarkup $ [map cons_num [1, 2, 3, 4, 5]]
  where
    cons_num :: Int -> InlineKeyboardButton
    cons_num (showt -> n) = InlineKeyboardButton n n
