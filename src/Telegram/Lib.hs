module Telegram.Lib
  ( startServer,
  )
where

import Config (Settings (..))
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (replicateM_)
import Data.Aeson
  ( KeyValue ((.=)),
    eitherDecode,
    encode,
    object,
  )
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
  ( RequestBody (..),
    Response (..),
    defaultManagerSettings,
    httpLbs,
    method,
    parseRequest,
    requestBody,
    requestHeaders,
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
  ( getUpdates,
    keyboardJSON,
    sendJSON,
    sendSticker,
    sendText,
  )
import Telegram.Types (ChatId, Env (..), MessageRequest (..), RawResponse, Token)
import Text.Read (readMaybe)
import TextShow (TextShow (showt))
import Prelude hiding (log, putStr, putStrLn, readFile, writeFile)

messageId :: Message -> ChatId
messageId = chat_id . chat

-- обработка пользоватских действий
whichMessage :: Message -> (MessageRequest, ChatId)
whichMessage mes = (messageRequest, messageId mes)
  where
    messageRequest = case text mes of
      Just txt -> whichText txt
      Nothing -> case sticker mes of
        Just st -> Stick $ sticker_file_id st
        Nothing -> Mes "unsupported because this is not a text or sticker"

action :: Env -> MessageRequest -> ChatId -> IO ()
action env@Env {..} mes chatid = case mes of
  Stick st -> sendMessage chatid $ sendSticker token chatid st
  Mes txt -> sendMessage chatid $ sendText token chatid txt
  Start -> newRepeat defaultRepeat "usersTG" chatid
  Help -> sendMessage chatid $ sendText token chatid help
  Repeat -> sendMessageKeyBoard env chatid

{-
todo

1. Пользователь может отправить команду /repeat
и в ответ бот отправит какое
сейчас выбрано значение повторов и вопрос,
сколько раз повторять сообщение в дальнейшем

-}

whichText :: Text -> MessageRequest
whichText "/start" = Start
whichText "/help" = Help
whichText "/repeat" = Repeat
whichText text = Mes text

-- отправлет один раз сообщение
sendMessageOne :: RawResponse -> IO ()
sendMessageOne raw = do
  putStrLn "sendMessageOne"
  manager <- getGlobalManager
  resp <- parseRequest (T.unpack $ raw) >>= flip httpLbs manager
  print resp

sendMessageKeyBoard :: Env -> ChatId -> IO ()
sendMessageKeyBoard Env {..} chatId = do
  sendMessage chatId $ sendText token chatId repeat
  n <- queryRepeat "usersTG" chatId
  sendMessage chatId $ sendText token chatId $ "now you have " <> showt n
  manager <- getGlobalManager
  initialRequest <- parseRequest (T.unpack $ sendJSON token)
  let requestObject =
        object
          [ "chat_id" .= chatId,
            "text" .= ("repeat" :: Text),
            "reply_markup" .= keyboardJSON,
            "one_time_keyboard" .= True
          ]
  let request =
        initialRequest
          { method = "POST",
            requestHeaders = [("Content-Type", "application/json; charset=utf-8")],
            requestBody = RequestBodyLBS $ encode requestObject
          }
  print request
  response <- httpLbs request manager
  print response

sendMessage :: ChatId -> RawResponse -> IO ()
sendMessage chatId raw = do
  n <- queryRepeat "usersTG" chatId
  replicateM_ n (sendMessageOne raw)
  print raw

actionUpdate :: Env -> Update -> IO ()
actionUpdate env@Env {..} update = do
  print update
  let actionM = (message update) <&> (uncurry (action env) . whichMessage)
  print (isNothing actionM)
  if (isNothing actionM)
    then do
      print "haveCallBack"
      haveCallBack
    else do
      print "actionM"
      fromJust actionM
  print "endActionUpdate"
  where
    haveCallBack = case callback_query update of
      Nothing -> putStrLn "this not a message and what i need to do???"
      Just query -> haveData query

    haveData query = case cq_data query of
      Nothing -> putStrLn "we have callbackquery, but it no have data for us"
      Just txt -> haveMessage query txt

    haveMessage query txt = case (fmap messageId) (cq_message query) of
      Nothing -> putStrLn "we have callbackquery, but it no have message for us"
      Just chatId -> haveNumber txt chatId

    haveNumber txt chatId = case readMaybe (T.unpack txt) of
      Nothing -> putStrLn "this not a number???"
      Just n -> allHave txt chatId n

    allHave txt chatId n = do
      updateRepeat table chatId n
      let repeatN = "received " `T.append` txt
      let raw = sendText token chatId repeatN
      sendMessage chatId raw

loopUpdate :: Env -> IO ()
loopUpdate env@Env {..} = do
  threadDelay (3 * 1000000)
  let request =
        getUpdates token
          `T.append` "?offset="
          `T.append` showt offset
  manager <- getGlobalManager
  resp <- parseRequest (T.unpack request) >>= flip httpLbs manager

  print resp
  print "blayt\n"

  case eitherDecode (responseBody resp) of
    Left err -> do
      print "eitherDecode"
      print err
    Right up ->
      case result up of
        [] -> loopUpdate env
        ups -> do
          let offset = update_id $ last ups
          mapM_ (actionUpdate env) ups
          writeFile path (showt $ offset + 1)
          loopUpdate $ env {offset = offset + 1}

startServer :: Config -> Settings -> IO ()
startServer Config {..} Settings {..} = do
  text <- readFile path
  print text
  let offset = fromMaybe (-1 :: Int) (readMaybe (T.unpack text))
  let table = fromString tableString
  let env = Env {..}
  loopUpdate env
