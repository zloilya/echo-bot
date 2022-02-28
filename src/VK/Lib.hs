module VK.Lib
  ( startServer,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Lens ((^.), (^?))
import Control.Monad (replicateM_)
import Data.Aeson (decode, eitherDecode, encode)
--import Data.Aeson.Lens (key)
import Data.Aeson.Types
  ( FromJSON (..),
    GFromJSON,
    GToJSON,
    Options (..),
    Parser,
    SumEncoding (..),
    ToJSON (..),
    Value (String),
    Zero,
    defaultOptions,
    genericParseJSON,
    genericToJSON,
    withObject,
    (.:),
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as LB
import Data.Functor ((<&>))
import Data.Int (Int16, Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only),
    close,
    connectPostgreSQL,
    execute,
    query,
  )
import Debug.Trace (trace)
import GHC.Generics (Generic (..))
import Network.HTTP.Client (Manager, Response (..), defaultManagerSettings, httpLbs, method, newManager, parseRequest)
import Network.HTTP.Client.TLS (tlsManagerSettings, setGlobalManager, getGlobalManager)
import Network.HTTP.Types (hContentType, status200)
import Security (accessToken, chatID, groupId, token)
import System.Random (newStdGen, randomIO, uniformR)
import Text.Read (readMaybe)
import TextShow (TextShow (showt))
import qualified VK.Api as Api
import Prelude hiding (log, putStr, putStrLn)

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
getUpdates long@Api.LongPollServer {..} =
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

post :: Text -> IO (Response LB.ByteString)
post response = do 
  manager <- getGlobalManager
  resp    <- parseRequest (T.unpack response)
  httpLbs resp manager

actionUpdate :: Api.Update -> IO ()
actionUpdate update = do
  let Api.Update {update_object = object} = update
  let Api.Object {object_message = message} = object
  let Api.Message
        { message_from_id = userId,
          message_text = text,
          message_attachments = attachments,
          message_peer_id = peerId
        } = message
  print "well update"
  rand <- randomIO :: IO Int64
  case attachments of
    [] -> do
      print "i think is message"
      let send = sendMessage userId text rand
      resp <- post send
      print "wow"
      print resp
      putStrLn text
    at : _ -> do
      print "wow sticker!!!!"
      let Api.Attachments {atta_sticker = sticker} = at
      let Api.Sticker {sticker_id = stickerId} = sticker
      let send = sendSticker userId rand peerId stickerId
      resp <- post send
      print resp
      print "bruch"

loopUpdates :: Api.LongPollServer -> IO ()
loopUpdates lps = do
  print "hello"
  threadDelay (2 * 1000000)
  resp_updates <- post (getUpdates lps)
  print resp_updates
  case eitherDecode (responseBody resp_updates) of
    Left s -> do
      print "error when decode Update"
      print s
    Right (Api.Ok ts updates) -> do
      mapM_ actionUpdate updates
      loopUpdates lps {Api.long_ts = ts}
  putStrLn "я офигел честно"

startServer :: IO ()
startServer = do
  print "start"
  manager <- newManager tlsManagerSettings
  setGlobalManager manager
  resp <- post getLongPollServer
  let body = responseBody resp
  print resp
  print body
  case eitherDecode body of
    Left s -> do
      print "error when decode LongPollServer"
      print s
    Right vkresp -> do
      let Api.VKResponse {response = lps} = vkresp
      print "well done"
      set_resp <- post setLongPollSettings
      print set_resp
      loopUpdates lps
