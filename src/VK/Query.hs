module VK.Query
  ( getLongPollServer,
    setLongPollSettings,
    getUpdates,
    requestKeyBoard,
    requestMessage,
    requestSticker,
  )
where

import Data.Aeson (encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client
  ( Request,
    setQueryString,
  )
import TextShow (TextShow (showt))
import VK.Api
  ( Action (..),
    Action1 (Action1),
    Button (..),
    KeyBoard (..),
    LongPollServer (..),
  )
import VK.Types
  ( Env (..),
    GroupId,
    PeerId,
    RandomId,
    StikerId,
    Token,
    UserId,
  )

{-
we often need this three field
-}
groupAccessVersion :: Token -> GroupId -> Text
groupAccessVersion token groupId =
  T.concat
    [ "?group_id=" `T.append` groupId,
      "&access_token=" `T.append` token,
      "&v=5.131"
    ]

{-
vk send us LongPollServer
-}
getLongPollServer :: Token -> Text -> GroupId -> Text
getLongPollServer token api groupId =
  T.concat
    [ api,
      "/groups.getLongPollServer",
      groupAccessVersion token groupId
    ]

{-
vk filter for us messages
-}
setLongPollSettings :: Token -> Text -> GroupId -> Text
setLongPollSettings token api groupId =
  T.concat
    [ api,
      "/groups.setLongPollSettings",
      groupAccessVersion token groupId,
      "&enabled=1",
      "&message_new=1"
    ]

{-
get updates
-}
getUpdates :: LongPollServer -> Text
getUpdates LongPollServer {..} =
  T.concat
    [ long_server,
      "?act=a_check",
      "&key=" `T.append` long_key,
      "&ts=" `T.append` long_ts,
      "&wait=25"
    ]

{-
helper function
-}
setQuery :: Text -> Maybe ByteString
setQuery = Just . encodeUtf8

{-
preparation to send message
-}
requestMessage :: Env -> UserId -> Text -> RandomId -> Request -> Request
requestMessage Env {..} userId text randomId initialRequest = do
  let request =
        setQueryString
          [ ("group_id", setQuery groupId),
            ("access_token", setQuery token),
            ("v", setQuery "5.131"),
            ("user_id", setQuery $ showt userId),
            ("random_id", setQuery $ showt randomId),
            ("message", setQuery text)
          ]
          initialRequest
  request

{-
preparation to send sticker
-}
requestSticker :: Env -> UserId -> PeerId -> StikerId -> RandomId -> Request -> Request
requestSticker Env {..} userId peerId stickerId randomId initialRequest = do
  let request =
        setQueryString
          [ ("access_token", setQuery token),
            ("v", setQuery "5.131"),
            ("user_id", setQuery $ showt userId),
            ("random_id", setQuery $ showt randomId),
            ("peer_id", setQuery $ showt peerId),
            ("sticker_id", setQuery $ showt stickerId)
          ]
          initialRequest
  request

{-
preparation to send keyboard
-}
requestKeyBoard :: Env -> UserId -> PeerId -> RandomId -> Request -> Request
requestKeyBoard Env {..} userId peerId randomId initialRequest = do
  let request =
        setQueryString
          [ ("access_token", Just . encodeUtf8 $ token),
            ("v", Just . encodeUtf8 $ "5.131"),
            ("user_id", Just . encodeUtf8 $ showt userId),
            ("random_id", Just . encodeUtf8 $ showt randomId),
            ("peer_id", Just . encodeUtf8 $ showt peerId),
            ("message", Just . encodeUtf8 $ "repeat"),
            ("keyboard", Just . toStrict . encode $ keyboard)
          ]
          initialRequest
  request

{-
it is a keyboard
-}
keyboard :: KeyBoard
keyboard =
  KeyBoard True [map cons_num [1, 2, 3, 4, 5]]
  where
    cons_num :: Int -> Action1
    cons_num (showt -> n) = Action1 $ Action "text" (Button n) n
