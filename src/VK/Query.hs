module VK.Query
  ( apiVk,
    groupAccessVersion,
    getLongPollServer,
    setLongPollSettings,
    getUpdates,
    sendMessage,
    sendSticker,
    sendJSON,
    keyboardJSON,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import TextShow (TextShow (showt))
import qualified VK.Api as Api
import VK.Types
  ( Env (..),
    GroupId,
    PeerId,
    RandomId,
    StikerId,
    Token,
    UserId,
  )

apiVk :: Text
apiVk = "https://api.vk.com/method/"

groupAccessVersion :: Token -> GroupId -> Text
groupAccessVersion token groupId =
  T.concat
    [ "?group_id=" `T.append` groupId,
      "&access_token=" `T.append` token,
      "&v=5.131"
    ]

getLongPollServer :: Token -> GroupId -> Text
getLongPollServer token groupId =
  T.concat
    [ apiVk,
      "groups.getLongPollServer",
      groupAccessVersion token groupId
    ]

setLongPollSettings :: Token -> GroupId -> Text
setLongPollSettings token groupId =
  T.concat
    [ apiVk,
      "groups.setLongPollSettings",
      groupAccessVersion token groupId,
      "&enabled=1",
      "&message_new=1"
    ]

getUpdates :: Api.LongPollServer -> Text
getUpdates Api.LongPollServer {..} =
  T.concat
    [ long_server,
      "?act=a_check",
      "&key=" `T.append` long_key,
      "&ts=" `T.append` long_ts,
      "&wait=25"
    ]

sendMessage :: Env -> UserId -> Text -> RandomId -> Text
sendMessage Env {..} userId text randomId =
  T.concat
    [ apiVk,
      "messages.send",
      groupAccessVersion token groupId,
      "&user_id=" `T.append` showt userId,
      "&random_id=" `T.append` showt randomId,
      "&message=" `T.append` text
    ]

sendSticker :: Env -> UserId -> PeerId -> StikerId -> RandomId -> Text
sendSticker Env {..} userId peerId stickerId randomId =
  T.concat
    [ apiVk,
      "messages.send",
      "?access_token=" `T.append` token,
      "&v=5.131",
      "&user_id=" `T.append` showt userId,
      "&random_id=" `T.append` showt randomId,
      "&peer_id=" `T.append` showt peerId,
      "&sticker_id=" `T.append` showt stickerId
    ]

sendJSON :: Text
sendJSON =
  T.concat
    [ apiVk,
      "messages.send"
    ]

keyboardJSON :: Api.KeyBoard
keyboardJSON =
  Api.KeyBoard
    { one_time = True,
      buttons = [map cons_num [1, 2, 3, 4, 5]]
    }
  where
    cons_num :: Int -> Api.Action1
    cons_num (showt -> n) =
      Api.Action1 $
        Api.Action
          { act_type = "text",
            act_payload = Api.Button {button = n},
            act_label = n
          }
