module VK.Lib
  ( startServer,
  )
where

import Control.Concurrent (threadDelay)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LB
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import GHC.Generics (Generic (..))
import Network.HTTP.Client
  ( Manager,
    Response (..),
    defaultManagerSettings,
    httpLbs,
    method,
    newManager,
    parseRequest,
  )
import Network.HTTP.Client.TLS (getGlobalManager, setGlobalManager, tlsManagerSettings)
import Security (accessToken, groupId, token)
import System.Random (randomIO)
import TextShow (TextShow (showt))
import VK.Api
  ( Attachments (..),
    LongPollServer (..),
    Message (..),
    Object (..),
    Ok (..),
    Sticker (..),
    Update (..),
    VKResponse (..),
  )
import VK.Query
  ( getLongPollServer,
    getUpdates,
    sendMessage,
    sendSticker,
    setLongPollSettings,
  )
import Prelude hiding (putStrLn)

post :: Text -> IO (Response LB.ByteString)
post response = do
  manager <- getGlobalManager
  resp <- parseRequest (T.unpack response)
  httpLbs resp manager

{-
todo

1. Пользователь может отправить команду /repeat
и в ответ бот отправит какое
сейчас выбрано значение повторов и вопрос,
сколько раз повторять сообщение в дальнейшем

-}

actionUpdate :: Update -> IO ()
actionUpdate update = do
  let Update {update_object = object} = update
  let Object {object_message = message} = object
  let Message
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
      let Attachments {atta_sticker = sticker} = at
      let Sticker {sticker_id = stickerId} = sticker
      let send = sendSticker userId rand peerId stickerId
      resp <- post send
      print resp
      print "bruch"

loopUpdates :: LongPollServer -> IO ()
loopUpdates lps = do
  print "hello"
  threadDelay (2 * 1000000)
  resp_updates <- post (getUpdates lps)
  print resp_updates
  case eitherDecode (responseBody resp_updates) of
    Left s -> do
      print "error when decode Update"
      print s
    Right (Ok ts updates) -> do
      mapM_ actionUpdate updates
      loopUpdates lps {long_ts = ts}
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
      let VKResponse {response = lps} = vkresp
      print "well done"
      set_resp <- post setLongPollSettings
      print set_resp
      loopUpdates lps
