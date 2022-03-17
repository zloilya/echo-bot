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
import Data.Aeson.Types (FromJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as LB
import Data.Functor (void)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO (putStrLn)
import GHC.Generics (Generic (..))
import Network.HTTP.Client
  ( Manager,
    Request,
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
import PostgresQuery (Table, newRepeat, queryRepeat, updateRepeat)
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
    requestKeyBoard,
    requestMessage,
    requestSticker,
    sendJSON,
    setLongPollSettings,
  )
import VK.Types (ChatId, Env (..), PeerId, RandomId, UserId)
import Prelude hiding (putStrLn)

{-
метод принимает готовый респонс и отправляет его
-}
post :: Text -> IO (Response LB.ByteString)
post request = do
  manager <- getGlobalManager
  resp <- parseRequest (T.unpack request)
  httpLbs resp manager

{-
отправляет один реквест
-}
sendOne :: (RandomId -> Request -> Request) -> IO ()
sendOne send = do
  randomId <- randomIO
  manager <- getGlobalManager
  initRequest <- parseRequest (T.unpack sendJSON)
  let request = send randomId initRequest
  print =<< httpLbs request manager

{-
отправляет n раз реквест
-}
sendMany :: Table -> UserId -> (RandomId -> Request -> Request) -> IO ()
sendMany table userId send = do
  n <- queryRepeat table userId
  replicateM_ n (sendOne send)
  print "end sendMany"

{-
много интересного может храть payload если он пришел
если Command start то newRepeat
если Button num то updateRepeat
-}
actionCommand :: Env -> Maybe Text -> ChatId -> IO ()
actionCommand Env {..} payload userId = case payload of
  Nothing -> print "no payload"
  Just txt -> case eitherDecode (fromStrict . encodeUtf8 $ txt) of
    Left s1 -> case eitherDecode @Command (fromStrict . encodeUtf8 $ txt) of
      Left s2 -> print s1 >> print s2
      Right com ->
        if command com == "start"
          then newRepeat defaultRepeat table userId
          else print "this command not a start???"
    Right (Button textNum) -> do
      let num = fromMaybe (1 :: Int) (readMaybe (T.unpack textNum))
      updateRepeat table userId num
      print ("yes: " ++ show num)

{-
по update выполняет действие, если update один из ожидаемых
-}
actionUpdate :: Env -> Update -> IO ()
actionUpdate env@Env {..} (Update (Object message)) = do
  let Message userId attachments peerId text payload = message
  print "well update"
  void $ actionCommand env payload userId
  case attachments of
    [] -> do
      print "i think is message"
      if text == "/repeat"
        then do
          print "its repeat"
          let send = requestKeyBoard env userId peerId
          sendOne send
        else do
          print "wow message!!!!"
          let send = requestMessage env userId text
          sendMany table userId send
    ats -> mapM_ sendSt ats
      where
        sendSt (Attachments (Sticker stickerId)) = do
          print "wow sticker!!!!"
          let send = requestSticker env userId peerId stickerId
          sendMany table userId send

{-
посылает реквест и декодирует ответ, бросает ошибку
(до этого просто возвращала Nothing, и тоже завершала программу)
-}
requestDecode :: FromJSON a => Text -> IO a
requestDecode request = do
  resp <- post request
  case eitherDecode (responseBody resp) of
    Right a -> pure a
    _ -> error "unknown vk response"

{-
раз в 3 секунды запрашивает у телеграмма список update и страется их выполнить
-}
handleUpdate :: Env -> (Update -> IO b) -> IO a
handleUpdate env@Env {..} action = do
  print "loopUpdate"
  threadDelay (3 * 1000000)
  let request = getUpdates lps
  Ok ts updates <- requestDecode request
  mapM_ action updates
  handleUpdate env {lps = lps {long_ts = ts}} action

{-
по конфигу и настройкам заупскает бота для вконтакте
-}
startServer :: Config -> Settings -> IO ()
startServer Config {..} Settings {..} = do
  print "start"
  let table = fromString tableString
  let request = getLongPollServer token groupId
  VKResponse lps <- requestDecode request
  print "well done"
  set_resp <- post $ setLongPollSettings token groupId
  print set_resp
  let env = Env {..}
  handleUpdate env (actionUpdate env)
