module VK.Query
  ( apiVk,
    groupAccessVersion,
    getLongPollServer,
    setLongPollSettings,
    getUpdates,
    sendMessage,
    sendSticker,
  )
where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Security (accessToken, groupId, token)
import TextShow (TextShow (showt))
import qualified VK.Api as Api

apiVk :: Text
apiVk = "https://api.vk.com/method/"

groupAccessVersion :: Text
groupAccessVersion =
  T.concat
    [ "?group_id=" `T.append` groupId,
      "&access_token=" `T.append` accessToken,
      "&v=5.131"
    ]

getLongPollServer :: Text
getLongPollServer =
  T.concat
    [ apiVk,
      "groups.getLongPollServer",
      groupAccessVersion
    ]

setLongPollSettings :: Text
setLongPollSettings =
  T.concat
    [ apiVk,
      "groups.setLongPollSettings",
      groupAccessVersion,
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

sendMessage :: Int64 -> Text -> Int64 -> Text
sendMessage userId text rand =
  T.concat
    [ apiVk,
      "messages.send",
      groupAccessVersion,
      "&user_id=" `T.append` showt userId,
      "&random_id=" `T.append` showt rand,
      "&message=" `T.append` text
    ]

sendSticker :: Int64 -> Int64 -> Int64 -> Int64 -> Text
sendSticker userId randomId peerId stickerId =
  T.concat
    [ apiVk,
      "messages.send",
      "?access_token=" `T.append` accessToken,
      "&v=5.131",
      "&user_id=" `T.append` showt userId,
      "&random_id=" `T.append` showt randomId,
      "&peer_id=" `T.append` showt peerId,
      "&sticker_id=" `T.append` showt stickerId
    ]
