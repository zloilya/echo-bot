module Telegram.Query
  ( apiTg,
    sendText,
    sendSticker,
    sendJSON,
    getUpdates,
    keyboardJSON,
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Security (accessToken, groupId, token)
import qualified Telegram.Api as Api
import Telegram.Сommon (Env (..))
import TextShow (TextShow (showt))

-- базовая сылка телеги
apiTg :: Text
apiTg = "https://api.telegram.org/bot" `T.append` token

-- реквест на отправку текста
sendText :: Env -> Text
sendText env@Env {..} =
  T.concat
    [ apiTg,
      "/sendMessage?chat_id=" `T.append` showt env_chat_id,
      "&text=" `T.append` env_response_text
    ]

-- реквест на отправку стикера
sendSticker :: Env -> Text
sendSticker env@Env {..} =
  T.concat
    [ apiTg,
      "/sendSticker",
      "?chat_id=" `T.append` showt env_chat_id,
      "&sticker=" `T.append` env_response_text
    ]

sendJSON :: Text
sendJSON = apiTg `T.append` "/sendMessage"

getUpdates :: Text
getUpdates = apiTg `T.append` "/getUpdates"

keyboardJSON :: Api.InlineKeyboardMarkup
keyboardJSON = Api.InlineKeyboardMarkup $ [map cons_num [1, 2, 3, 4, 5]]
  where
    cons_num :: Int -> Api.InlineKeyboardButton
    cons_num (showt -> n) =
      Api.InlineKeyboardButton
        { ikb_text = n,
          ikb_callback_data = Just $ n
        }
