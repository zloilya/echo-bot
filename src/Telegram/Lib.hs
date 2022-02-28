module Telegram.Lib
  ( startServer,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (replicateM_)
import Data.Aeson (decode, eitherDecode, encode)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as LB
import Data.Functor ((<&>))
import Data.Int (Int32, Int64)
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
import GHC.Generics (Generic)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest, setRequestBodyJSON)
import Network.HTTP.Types (hContentType, status200)
import Security (chatID, token)
import qualified Telegram.Api as Api
import Text.Read (readMaybe)
import TextShow (TextShow (showt))
import Prelude hiding (log, putStr, putStrLn)

data SendJSON = SendJSON
  { chat_id :: Int64,
    text :: Text,
    reply_markup :: Api.InlineKeyboardMarkup,
    one_time_keyboard :: Bool
  }
  deriving (ToJSON, Show, Generic)

data Ok a = Ok
  { ok :: Bool,
    result :: a
  }
  deriving (FromJSON, Show, Generic)

-- удобна не редачить сигнатуру для функций
data Env = Env
  { chat_id_f :: Int64,
    response_text :: Text,
    send :: Env -> Text
  }

-- базовая сылка телеги
apiTg :: Text
apiTg = "https://api.telegram.org/bot" `T.append` token

-- реквест на отправку текста
sendText :: Env -> Text
sendText env@Env {..} =
  T.concat
    [ apiTg,
      "/sendMessage?chat_id=",
      showt chat_id_f,
      "&text=",
      response_text
    ]

-- реквест на отправку стикера
sendSticker :: Env -> Text
sendSticker env@Env {..} =
  T.concat
    [ apiTg,
      "/sendSticker?chat_id=",
      showt chat_id_f,
      "&sticker=",
      response_text
    ]

sendJ :: Text
sendJ =
  T.concat
    [ apiTg,
      "/sendMessage"
    ]

getUpdates :: Text
getUpdates =
  T.concat
    [ apiTg,
      "/getUpdates"
    ]

-- резльтат обработки пользовательских действий
data MessageLib
  = Stick Text -- для стикеров
  | Mes Text -- для всех текстовых сообщений не являющимися командами
  | Repeat -- для /repeat
  | Start -- для /start

keyboardJ :: Api.InlineKeyboardMarkup
keyboardJ = Api.InlineKeyboardMarkup $ [map cons_num [1, 2, 3, 4, 5]]
  where
    cons_num :: Int -> Api.InlineKeyboardButton
    cons_num (showt -> n) =
      Api.InlineKeyboardButton
        { ikb_text = n,
          ikb_url = Nothing,
          ikb_callback_data = Just $ n,
          ikb_switch_inline_query = Nothing,
          ikb_callback_game = Nothing,
          ikb_switch_inline_query_current_chat = Nothing,
          ikb_pay = Nothing
        }

--Api.text
-- обработка пользоватских действий
whichMessage :: Maybe Api.Message -> MessageLib
whichMessage (Just mes) = case Api.text mes of
  Just txt -> whichText txt
  Nothing -> case Api.sticker mes of
    Just st -> Stick $ Api.sticker_file_id st
    Nothing -> Mes "unsupported because this is not a text or sticker"
whichMessage Nothing = Mes "unsupported because this is not a message"

whichText :: Text -> MessageLib
whichText "/start" = Start
whichText "/help" = Mes "my bot for metalamp"
whichText "/repeat" = Repeat
whichText text = Mes text

-- существует ли уже такой юзер
isAssigned :: Connection -> Int64 -> IO Bool
isAssigned conn id = query conn select (Only id) <&> not . null @_ @(Only Int64)
  where
    select = "SELECT id FROM users WHERE id = ?"

-- увеличивает count для юзера
updateCount :: Int64 -> Int -> Connection -> IO ()
updateCount id new_count conn = do
  bool <- isAssigned conn id
  print ("isAssigned: " ++ show bool)
  res <- case bool of
    True -> do
      let update = "UPDATE users set count = ? where id = ?"
      execute conn update (new_count, id)
    False -> do
      let insert = "INSERT INTO users (id, count) VALUES (?, ?)"
      execute conn insert (id, new_count)
  print ("execute: " ++ show res)

-- создает юзера или ставит ему значение 1
newRepeat :: Int64 -> IO ()
newRepeat id = updateRepeat id 1

-- создает юзера или ставит ему значение n
updateRepeat :: Int64 -> Int -> IO ()
updateRepeat = ((bracket (connectPostgreSQL connString) close) .) . updateCount
  where
    connString = "host=localhost dbname=metalamp"

-- обращается в базу за значением юзера
queryCount :: Int64 -> Connection -> IO Int
queryCount id conn = do
  ls <- query @_ @(Only Int) conn select (Only id)
  pure $ case ls of
    [(Only n)] -> n
    _ -> 1
  where
    select = "SELECT count FROM users WHERE id = ?"

-- запрос к базе metalamp queryCount обернытый в bracket на случай ошибки
queryText :: Int64 -> IO Int
queryText = bracket (connectPostgreSQL connString) close . queryCount
  where
    connString = "host=localhost dbname=metalamp"

-- отправлет один раз сообщение
sendMessageOne :: Env -> IO ()
sendMessageOne env@Env {..} = do
  putStrLn response_text
  resp <- parseRequest (T.unpack $ send env) >>= httpLBS
  print resp
  return ()

sendMessageJSON :: Env -> IO ()
sendMessageJSON env@Env {..} = do
  putStrLn response_text
  request <- parseRequest (T.unpack $ send env)
  let json = SendJSON chat_id_f response_text keyboardJ True
  resp <- httpLBS (setRequestBodyJSON json request)
  print resp
  return ()

--
sendMessage :: Env -> IO ()
sendMessage env@Env {..} = do
  n <- queryText chat_id_f
  replicateM_ n (sendMessageOne env)
  print response_text

chadId :: Api.Message -> Int64
chadId = Api.chat_id . Api.chat

actionUpdate :: Api.Update -> IO ()
actionUpdate update = do
  print (show update)
  case (fmap chadId) (Api.message update) of
    Nothing -> case Api.callback_query update of
      Nothing -> putStrLn "this not a message and what i need to do???"
      Just query -> case Api.cq_data query of
        Nothing -> putStrLn "we have callbackquery, but it no have data for us"
        Just txt -> case (fmap chadId) (Api.cq_message query) of
          Nothing -> putStrLn "we have callbackquery, but it no have message for us"
          Just chatid -> case readMaybe @Int (T.unpack txt) of
            Nothing -> putStrLn "this not a number???"
            Just n -> do
              updateRepeat chatid n
              sendMessage $ Env chatid ("received " `T.append` txt) sendText
    Just chatid ->
      let mes = whichMessage $ Api.message update
       in case mes of
            Stick st -> sendMessage $ Env chatid st sendSticker
            Mes txt -> sendMessage $ Env chatid txt sendText
            Repeat -> sendMessageJSON $ Env chatid "repeat" (\_ -> sendJ)
            Start -> newRepeat chatid

loopUpdate :: Int -> IO ()
loopUpdate offset = do
  threadDelay (3 * 1000000)
  let request = getUpdates `T.append` "?offset=" `T.append` showt offset
  resp <- parseRequest (T.unpack request) >>= httpLBS
  print resp
  print "blayt\n"

  case eitherDecode (getResponseBody resp) of
    Left err -> do
      print "eitherDecode"
      print err
    Right up ->
      case result up of
        [] -> loopUpdate offset
        ups -> do
          let offset = Api.update_id $ last ups
          mapM_ actionUpdate ups
          loopUpdate (offset + 1)

-- возможно стоить отфильтровать по уже существующему offset или сразу запустить с ним loop
startLoopUpdate :: IO ()
startLoopUpdate = do
  resp <- parseRequest (T.unpack getUpdates) >>= httpLBS
  print resp
  print "blayt\n"

  case eitherDecode (getResponseBody resp) of
    Left err -> do
      print "eitherDecode"
      print err
    Right up ->
      case result up of
        [] -> startLoopUpdate
        ups -> do
          let offset = Api.update_id $ last ups
          mapM_ actionUpdate ups
          loopUpdate (offset + 1)

-- loopUpdate update_id

startServer :: IO ()
startServer = do
  -- запуск сервера для Webhook
  -- run 8443 app
  -- запуск цикла для getUpdates
  startLoopUpdate
