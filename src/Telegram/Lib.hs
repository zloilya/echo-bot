module Telegram.Lib
  ( startServer,
  )
where

import Config (Settings (..))
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (replicateM_, void)
import Data.Aeson (FromJSON, Value, eitherDecode, encode)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as LB
import Data.Functor ((<&>))
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStrLn, readFile, writeFile)
import Network.HTTP.Client
  ( Request (..),
    RequestBody (..),
    Response (..),
    httpLbs,
    parseRequest,
  )
import Network.HTTP.Client.TLS (getGlobalManager)
import PostgresQuery
  ( newRepeat,
    queryRepeat,
    updateRepeat,
  )
import Telegram.Api
  ( CallbackQuery (..),
    Chat (..),
    Message (..),
    Ok (..),
    Sticker (..),
    Update (..),
  )
import Telegram.Config (Config (..))
import Telegram.Query
  ( api,
    valueKeyBoard,
    valueMessage,
    valueStiker,
    valueUpdate,
  )
import Telegram.Types
  ( ChatId,
    Env (..),
    MessageRequest (..),
    Table,
    Token,
  )
import Text.Read (readMaybe)
import TextShow (TextShow (showt))
import Prelude hiding (log, putStr, putStrLn, readFile, writeFile)

{-
method get chat_id from message
-}
messageId :: Message -> ChatId
messageId = chat_id . chat

{-
user send message and we react somehow
-}
whichText :: Text -> MessageRequest
whichText "/start" = Start
whichText "/help" = Help
whichText "/repeat" = Repeat
whichText text = Mes text

{-
user send message and we react somehow and
we ignore message which is not a text or sticker or command
-}
whichMessage :: Message -> (MessageRequest, ChatId)
whichMessage mes = (messageRequest, messageId mes)
  where
    messageRequest = case mes of
      MessageText _ txt -> whichText txt
      MessageSticker _ st -> Stick $ sticker_file_id st
      MessageUnknown _ -> Mes "unsupported because this is not a text or sticker"

{-
after processing message we need to do something
1) if sticker then send sticker n times
2) if text then send text n times
3) if /start then create user in a db
4) if /help then send text from config
5) if /repeat then send keyboard
-}
action :: Env -> MessageRequest -> ChatId -> IO ()
action env@Env {..} mes chatid = do
  let sendIO = sendN table chatid
  case mes of
    Stick st -> do
      let value = valueStiker chatid st
      let io = sendMessageOne token "/sendSticker" value
      void $ sendIO io
    Mes txt -> do
      let value = valueMessage chatid txt
      let io = sendMessageOne token "/sendMessage" value
      void $ sendIO io
    Start -> newRepeat defaultRepeat table chatid
    Help -> do
      let value = valueMessage chatid help
      let io = sendMessageOne token "/sendMessage" value
      void $ sendIO io
    Repeat -> do
      let value = valueKeyBoard chatid
      let valueQuestion = valueMessage chatid repeat
      let io = sendMessageOne token "/sendMessage" valueQuestion
      void $ sendIO io
      void $ sendMessageOne token "/sendMessage" value

{-
send message one time with token method and json
-}
sendMessageOne :: Token -> Text -> Value -> IO LB.ByteString
sendMessageOne token method requestObject = do
  putStrLn "sendMessageOne"
  manager <- getGlobalManager
  initialRequest <- parseRequest (T.unpack $ api token `T.append` method)
  let request =
        initialRequest
          { method = "POST",
            requestHeaders = [("Content-Type", "application/json; charset=utf-8")],
            requestBody = RequestBodyLBS $ encode requestObject
          }
  print method
  print requestObject
  print request
  resp <- httpLbs request manager
  print resp
  return (responseBody resp)

{-
send message n times
-}
sendN :: Table -> ChatId -> IO a -> IO ()
sendN table chatId io = do
  n <- queryRepeat table chatId
  replicateM_ n io

{-
now we have update and do something with it
if update have message then go to action
if update have callback then update value on db
if update unknown then do nothing :shrag:
-}
actionUpdate :: Env -> Update -> IO ()
actionUpdate env@Env {..} update = do
  print "startActionUpdate"
  print update
  case update of
    UpdateMessage _ mes -> do
      print "have message"
      uncurry (action env) . whichMessage $ mes
    UpdateCallBack _ (CallbackQuery mes txt) -> do
      print "have callback"
      let chatId = messageId mes
      case readMaybe (T.unpack txt) of
        Nothing -> print "no valid text"
        Just n -> do
          updateRepeat table chatId n
          let repeatN = "received " `T.append` (showt n)
          let value = valueMessage chatId repeatN
          let io = sendMessageOne token "/sendMessage" value
          sendN table chatId io
    UpdateUnknown _ -> do
      print "unknown update"
  print "endActionUpdate"

{-
send request and docode it, throw error, because we can't start
-}
requestDecode :: FromJSON a => Token -> Text -> Value -> IO a
requestDecode token method value = do
  body <- sendMessageOne token method value
  case eitherDecode body of
    Right (Ok {ok = True, result}) -> pure result
    _ -> error "unknown telegram response"

{-
every 3 second send to telegram request and update offset after it
-}
handleUpdate :: Env -> (Update -> IO b) -> IO a
handleUpdate env@Env {..} action = do
  threadDelay (3 * 1000000)
  let value = valueUpdate offset
  updates <- requestDecode token "/getUpdates" value
  mapM_ action updates
  let newOffset = case updates of
        [] -> offset
        _ -> maximum ((offset + 1) : [update_id update | update <- updates])
  writeFile path (showt $ newOffset)
  handleUpdate (env {offset = newOffset}) action

{-
with config and settings start telegram bot
-}
startServer :: Config -> Settings -> IO ()
startServer Config {..} Settings {..} = do
  text <- readFile path
  print text
  let offset = fromMaybe (0 :: Int) (readMaybe (T.unpack text))
  let table = fromString tableString
  let env = Env {..}
  handleUpdate env (actionUpdate env)
