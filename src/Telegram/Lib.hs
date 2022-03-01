module Telegram.Lib
  ( startServer,
  )
where

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
import Data.Int (Int64)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStrLn, readFile, writeFile)
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
  )
import Network.HTTP.Client.TLS (getGlobalManager, setGlobalManager, tlsManagerSettings)
import Telegram.Api
  ( CallbackQuery (..),
    Chat (..),
    Message (..),
    Ok (..),
    Sticker (..),
    Update (..),
  )
import PostgresQuery
  ( newRepeat,
    queryText,
    updateRepeat,
  )
import Telegram.Query
  ( getUpdates,
    keyboardJSON,
    sendJSON,
    sendSticker,
    sendText,
  )
import Telegram.Common (Env (..))
import Text.Read (readMaybe)
import TextShow (TextShow (showt))
import Prelude hiding (log, putStr, putStrLn, readFile, writeFile)

-- резльтат обработки пользовательских действий
data MessageRequest
  = Stick Text -- для стикеров
  | Mes Text -- для всех текстовых сообщений не являющимися командами
  | Repeat -- для /repeat
  | Start -- для /start

messageId :: Message -> Int64
messageId = chat_id . chat

-- обработка пользоватских действий
whichMessage :: Message -> (MessageRequest, Int64)
whichMessage mes = (messageRequest, messageId mes)
  where
    messageRequest = case text mes of
      Just txt -> whichText txt
      Nothing -> case sticker mes of
        Just st -> Stick $ sticker_file_id st
        Nothing -> Mes "unsupported because this is not a text or sticker"

action :: Manager -> MessageRequest -> Int64 -> IO ()
action manager mes chatid = case mes of
  Stick st -> sendMessage manager $ Env chatid st sendSticker
  Mes txt -> sendMessage manager $ Env chatid txt sendText
  Repeat -> sendMessageJSON manager $ Env chatid "repeat" (\_ -> sendJSON)
  Start -> newRepeat chatid


{-
todo


1. Пользователь может отправить команду /repeat 
и в ответ бот отправит какое 
сейчас выбрано значение повторов и вопрос, 
сколько раз повторять сообщение в дальнейшем


-}






whichText :: Text -> MessageRequest
whichText "/start" = Start
whichText "/help" = Mes "my bot for metalamp"
whichText "/repeat" = Repeat
whichText text = Mes text

-- отправлет один раз сообщение
sendMessageOne :: Manager -> Env -> IO ()
sendMessageOne manager env@Env {..} = do
  putStrLn env_response_text
  resp <- parseRequest (T.unpack $ env_send env) >>= flip httpLbs manager
  print resp

sendMessageJSON :: Manager -> Env -> IO ()
sendMessageJSON manager env@Env {..} = do
  putStrLn env_response_text
  initialRequest <- parseRequest (T.unpack $ env_send env)
  let requestObject =
        object
          [ "chat_id" .= env_chat_id,
            "text" .= env_response_text,
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
  print (encode requestObject)
  response <- httpLbs request manager
  print response

sendMessage :: Manager -> Env -> IO ()
sendMessage manager env@Env {..} = do
  n <- queryText env_chat_id
  replicateM_ n (sendMessageOne manager env)
  print env_response_text

actionUpdate :: Manager -> Update -> IO ()
actionUpdate manager update = do
  print update
  let actionM = (message update) <&> (uncurry (action manager) . whichMessage)
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
      updateRepeat chatId n
      let repeatN = "received " `T.append` txt
      let env = Env chatId repeatN sendText
      sendMessage manager env

loopUpdate :: Manager -> Int -> IO ()
loopUpdate manager offset = do
  threadDelay (3 * 1000000)
  let request = getUpdates `T.append` "?offset=" `T.append` showt offset
  resp <- parseRequest (T.unpack request) >>= flip httpLbs manager

  print resp
  print "blayt\n"

  case eitherDecode (responseBody resp) of
    Left err -> do
      print "eitherDecode"
      print err
    Right up ->
      case result up of
        [] -> loopUpdate manager offset
        ups -> do
          let offset = update_id $ last ups
          mapM_ (actionUpdate manager) ups
          writeFile "src/lastOffset" (showt $ offset + 1)
          loopUpdate manager (offset + 1)

startServer :: IO ()
startServer = do
  manager <- newManager tlsManagerSettings
  let path = "src/lastOffset"
  text <- readFile path
  print text
  let offset = fromMaybe (-1 :: Int) (readMaybe (T.unpack text))
  loopUpdate manager offset
