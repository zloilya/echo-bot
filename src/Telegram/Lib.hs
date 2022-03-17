module Telegram.Lib
  ( startServer,
  )
where

import Config (Settings (..))
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (replicateM_, void)
import Data.Aeson
  ( KeyValue ((.=)),
    Value,
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
метод получения из меседжа чат айди
-}
messageId :: Message -> ChatId
messageId = chat_id . chat

{-
пользователь отправляет сообщение нам надо понять как реагировать
-}
whichText :: Text -> MessageRequest
whichText "/start" = Start
whichText "/help" = Help
whichText "/repeat" = Repeat
whichText text = Mes text

{-
обработка пользоватских действий из доступных текст и стикеры
-}
whichMessage :: Message -> (MessageRequest, ChatId)
whichMessage mes = (messageRequest, messageId mes)
  where
    messageRequest = case mes of
      MessageText _ txt -> whichText txt
      MessageSticker _ st -> Stick $ sticker_file_id st
      MessageUnknown _ -> Mes "unsupported because this is not a text or sticker"

{-
после обработки сообщения реагируем одним из представленных способов, а именно
1) если стикер то шлем стикер n раз
2) если текст то шлем текст n раз
3) если /start то добавляем в базу
4) если /help то шлем сообщение из конфига
5) если /repeat то шлем клавиатуру
-}
action :: Env -> MessageRequest -> ChatId -> IO ()
action env@Env {..} mes chatid = do
  let sendIO = sendN table chatid
  case mes of
    Stick st -> do
      let value = valueStiker chatid st
      let io = sendMessageOne token "/sendSticker" value
      sendIO io
    Mes txt -> do
      let value = valueMessage chatid txt
      let io = sendMessageOne token "/sendMessage" value
      sendIO io
    Start -> newRepeat defaultRepeat table chatid
    Help -> do
      let value = valueMessage chatid help
      let io = sendMessageOne token "/sendMessage" value
      sendIO io
    Repeat -> do
      let value = valueKeyBoard chatid
      void $ sendMessageOne token "/sendMessage" value

{-
отправлет один раз сообщение по зарание составленному респонсу
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
отправляет сообщение n раз
-}
sendN :: Table -> ChatId -> IO a -> IO ()
sendN table chatId io = do
  n <- queryRepeat table chatId
  replicateM_ n io

{-
по update выполняет действие, если update один из ожидаемых
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
посылает реквест и декодирует ответ, бросает ошибку
(до этого просто возвращала Nothing, и тоже завершала программу)
-}
requestDecode :: FromJSON a => Token -> Text -> Value -> IO a
requestDecode token method value = do
  body <- sendMessageOne token method value
  case eitherDecode body of
    Right (Ok {ok = True, result}) -> pure result
    _ -> error "unknown telegram response"

{-
раз в 3 секунды запрашивает у телеграмма список update и страется их выполнить
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
по конфигу и настройкам заупскает бота для телеграмма
-}
startServer :: Config -> Settings -> IO ()
startServer Config {..} Settings {..} = do
  text <- readFile path
  print text
  let offset = fromMaybe (0 :: Int) (readMaybe (T.unpack text))
  let table = fromString tableString
  let env = Env {..}
  handleUpdate env (actionUpdate env)
