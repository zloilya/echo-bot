module Telegram.Lib
  ( startServer,
  )
where

import Config (Settings (..), Database (..))
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
    Table,
    Postgres (..),
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
    Token,
  )
import Text.Read (readMaybe)
import TextShow (TextShow (showt))
import Prelude hiding (readFile, writeFile)
import Logger(Log(..), logFunctions)

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
      MessageSticker _ st -> Stick $ file_id st
      MessageUnknown _ -> Mes "unsupported because this is not a text or sticker"

{-
after processing message we need to do something
1) if sticker then send sticker n times
2) if text then send text n times
3) if /start then create user in a db
4) if /help then send text from config
5) if /repeat then send keyboard and question from config
-}
action :: Env -> MessageRequest -> ChatId -> IO ()
action env@Env {..} mes chatid = do
  let sendIO = sendN Postgres{..} chatid
  case mes of
    Stick st -> do
      logInfo "action: sticker"
      let value = valueStiker chatid st
      let io = sendOne env "/sendSticker" value
      void $ sendIO io
    Mes txt -> do
      logInfo "action: message"
      let value = valueMessage chatid txt
      let io = sendOne env "/sendMessage" value
      void $ sendIO io
    Start -> do
      logInfo "action: start"
      newRepeat Postgres{..} chatid
    Help -> do
      logInfo "action: help"
      let value = valueMessage chatid help
      let io = sendOne env "/sendMessage" value
      void $ sendIO io
    Repeat -> do
      logInfo "action: repeat"
      let value = valueKeyBoard chatid
      let valueQuestion = valueMessage chatid repeat
      let io = sendOne env "/sendMessage" valueQuestion
      void $ sendIO io
      void $ sendOne env "/sendMessage" value

{-
send message one time with token method and json
-}
sendOne :: Env -> Text -> Value -> IO LB.ByteString
sendOne Env {..} method requestObject = do
  logInfo $ "send " ++ (T.unpack method)
  logDebug "start sendOne"
  manager <- getGlobalManager
  initialRequest <- parseRequest (T.unpack $ api token `T.append` method)
  let request =
        initialRequest
          { method = "POST",
            requestHeaders = [("Content-Type", "application/json; charset=utf-8")],
            requestBody = RequestBodyLBS $ encode requestObject
          }
  logDebug $ "method:" ++ show method
  logDebug $ "requestObject:" ++ show requestObject
  logDebug $ "request:" ++ show request
  resp <- httpLbs request manager
  logDebug $ "response:" ++ show resp
  logDebug "end sendOne"
  return (responseBody resp)

{-
send message n times
-}
sendN :: Postgres -> ChatId -> IO a -> IO ()
sendN post chatId io = do
  n <- queryRepeat post chatId
  replicateM_ n io

{-
now we have update and do something with it
if update have message then go to action
if update have callback then update value on db
if update unknown then do nothing :shrag:
-}
actionUpdate :: Env -> Update -> IO ()
actionUpdate env@Env {..} update = do
  logInfo "startActionUpdate"
  logDebug $ "update:" ++ show update
  case update of
    UpdateMessage _ mes -> do
      logInfo "have message"
      uncurry (action env) . whichMessage $ mes
    UpdateCallBack _ (CallbackQuery mes txt) -> do
      logInfo "have callback"
      let chatId = messageId mes
      case readMaybe (T.unpack txt) of
        Nothing -> logWarn "no valid text"
        Just n -> do
          logInfo "good callback"
          updateRepeat Postgres{..} chatId n
          let repeatN = "received " `T.append` (showt n)
          let value = valueMessage chatId repeatN
          let io = sendOne env "/sendMessage" value
          sendN Postgres{..} chatId io
    UpdateUnknown _ -> do
      logWarn "unknown update"
  logInfo "endActionUpdate"

{-
send request and decode it, throw error, because we can't start
-}
requestDecode :: (Show a, FromJSON a) => Env -> Text -> Value -> IO a
requestDecode env method value = do
  body <- sendOne env method value
  case eitherDecode body of
    Right (Ok {ok = True, result}) -> pure result
    _e -> error $ "unknown telegram response:" ++ show _e

{-
every 3 second send to telegram request and update offset after it
-}
handleUpdate :: Env -> (Update -> IO b) -> IO a
handleUpdate env@Env {..} action = do
  logInfo "handle updates"
  threadDelay (3 * 1000000)
  let value = valueUpdate offset
  updates <- requestDecode env "/getUpdates" value
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
  let Database{..} = database
  let Log{..} = logFunctions loglevel
  logInfo "Start telegram server"
  text <- readFile path
  logInfo $ "read from file offset" ++ (T.unpack text)
  let offset = fromMaybe (0 :: Int) (readMaybe (T.unpack text))
  let table = fromString tableString
  let env = Env {..}
  logInfo "Start loop updates"
  handleUpdate env (actionUpdate env)
