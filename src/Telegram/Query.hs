module Telegram.Query
  ( apiTg,
    sendText,
    sendSticker,
    sendJSON,
    getUpdates,
    keyboardJSON,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Telegram.Api
  ( InlineKeyboardButton (..),
    InlineKeyboardMarkup (..),
  )
import Telegram.Types (ChatId, Token)
import TextShow (TextShow (showt))

-- базовая сылка телеги
apiTg :: Token -> Text
apiTg token = "https://api.telegram.org/bot" `T.append` token

-- реквест на отправку текста
sendText :: Token -> ChatId -> Text -> Text
sendText token chatId text =
  T.concat
    [ apiTg token,
      "/sendMessage",
      "?chat_id=" `T.append` showt chatId,
      "&text=" `T.append` text
    ]

-- реквест на отправку стикера
sendSticker :: Token -> ChatId -> Text -> Text
sendSticker token chatId text =
  T.concat
    [ apiTg token,
      "/sendSticker",
      "?chat_id=" `T.append` showt chatId,
      "&sticker=" `T.append` text
    ]

sendJSON :: Token -> Text
sendJSON token = apiTg token `T.append` "/sendMessage"

getUpdates :: Token -> Text
getUpdates token = apiTg token `T.append` "/getUpdates"

keyboardJSON :: InlineKeyboardMarkup
keyboardJSON = InlineKeyboardMarkup $ [map cons_num [1, 2, 3, 4, 5]]
  where
    cons_num :: Int -> InlineKeyboardButton
    cons_num (showt -> n) =
      InlineKeyboardButton
        { ikb_text = n,
          ikb_callback_data = Just $ n
        }
