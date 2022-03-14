module VK.Lib
  ( startServer,
  )
where

import Config (Settings (..))
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
import Data.Aeson
  ( KeyValue ((.=)),
    eitherDecode,
    encode,
    object,
  )
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as LB
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO (putStrLn)
import GHC.Generics (Generic (..))
import Network.HTTP.Client
  ( Manager,
    RequestBody (..),
    Response (..),
    defaultManagerSettings,
    httpLbs,
    method,
    newManager,
    parseRequest,
    requestBody,
    requestHeaders,
    setQueryString,
  )
import Network.HTTP.Client.TLS (getGlobalManager)
import PostgresQuery (newRepeat, queryRepeat, updateRepeat)
import System.Random (randomIO)
import Text.Read (readMaybe)
import TextShow (TextShow (showt))
import VK.Api
  ( Attachments (..),
    Button (..),
    Command (..),
    LongPollServer (..),
    Message (..),
    Object (..),
    Ok (..),
    Sticker (..),
    Update (..),
    VKResponse (..),
  )
import VK.Config (Config (..))
import VK.Query
  ( getLongPollServer,
    getUpdates,
    keyboardJSON,
    sendJSON,
    sendMessage,
    sendSticker,
    setLongPollSettings,
  )
import VK.Types (Env (..), PeerId, RandomId, UserId)
import Prelude hiding (putStrLn)

post :: Text -> IO (Response LB.ByteString)
post response = do
  manager <- getGlobalManager
  resp <- parseRequest (T.unpack response)
  httpLbs resp manager

sendOne :: (RandomId -> Text) -> IO ()
sendOne send = do
  randomId <- randomIO
  manager <- getGlobalManager
  let response = send randomId
  resp <- parseRequest (T.unpack response)
  print =<< httpLbs resp manager

sendMany :: UserId -> (RandomId -> Text) -> IO ()
sendMany userId send = do
  n <- queryRepeat "usersVK" userId
  replicateM_ n (sendOne send)
  print "end sendMany"

{-
todo

1. Пользователь может отправить команду /repeat
и в ответ бот отправит какое
сейчас выбрано значение повторов и вопрос,
сколько раз повторять сообщение в дальнейшем

2. все должны юзать setQueryString так как он энкодирует запросы

-}

sendMessageKeyBoard :: Env -> UserId -> PeerId -> IO ()
sendMessageKeyBoard Env {..} userId peerId = do
  print "send keyboard"
  manager <- getGlobalManager
  initialRequest <- parseRequest (T.unpack $ sendJSON)
  randomId <- randomIO :: IO RandomId
  let request =
        setQueryString
          [ ("access_token", Just . encodeUtf8 $ token),
            ("v", Just . encodeUtf8 $ "5.131"),
            ("user_id", Just . encodeUtf8 $ showt userId),
            ("random_id", Just . encodeUtf8 $ showt randomId),
            ("peer_id", Just . encodeUtf8 $ showt peerId),
            ("message", Just . encodeUtf8 $ "repeat"),
            ("keyboard", Just . toStrict . encode $ keyboardJSON)
          ]
          initialRequest
  print request
  print (encode keyboardJSON)
  response <- httpLbs request manager
  print response

actionUpdate :: Env -> Update -> IO ()
actionUpdate env@Env {..} update = do
  print update
  let Update {update_object = object} = update
  let Object {object_message = message} = object
  let Message
        { message_from_id = userId,
          message_text = text,
          message_attachments = attachments,
          message_peer_id = peerId,
          message_payload = payload
        } = message
  print "well update"
  case payload of
    Nothing -> print "no payload"
    Just txt -> case eitherDecode (fromStrict . encodeUtf8 $ txt) of
      Left s1 -> case eitherDecode @Command (fromStrict . encodeUtf8 $ txt) of
        Left s2 -> print s1 >> print s2
        Right com ->
          if command com == "start"
            then newRepeat 1 "usersVK" userId
            else print "this command not a start???"
      Right button -> do
        let Button {button = textNum} = button
        let num = fromMaybe (1 :: Int) (readMaybe (T.unpack textNum))
        updateRepeat "usersVK" userId num
        print ("yes: " ++ show num)
  case attachments of
    [] -> do
      print "i think is message"
      if text == "/repeat"
        then sendMessageKeyBoard env userId peerId
        else do
          print "wow message!!!!"
          let send = sendMessage env userId text
          sendMany userId send
    ats -> mapM_ sendSt ats
      where
        sendSt at = do
          print "wow sticker!!!!"
          let Attachments {atta_sticker = sticker} = at
          let Sticker {sticker_id = stickerId} = sticker
          let send = sendSticker env userId peerId stickerId
          sendMany userId send

loopUpdates :: Env -> IO ()
loopUpdates env@Env {..} = do
  print "hello"
  threadDelay (2 * 1000000)
  resp_updates <- post (getUpdates lps)
  print resp_updates
  case eitherDecode (responseBody resp_updates) of
    Left s -> do
      print "error when decode Update"
      print s
    Right (Ok ts updates) -> do
      mapM_ (actionUpdate env) updates
      loopUpdates env {lps = lps {long_ts = ts}}
  putStrLn "я офигел честно"

startServer :: Config -> Settings -> IO ()
startServer Config {..} Settings {..} = do
  print "start"
  resp <- post $ getLongPollServer token groupId
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
      set_resp <- post $ setLongPollSettings token groupId
      print set_resp
      loopUpdates $ Env {..}
