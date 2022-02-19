{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
module Lib
    ( someFunc
    ) where

--
import Network.Wai
import Network.HTTP.Simple (httpLBS, parseRequest)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import Debug.Trace (trace)
import qualified Api
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Data.ByteString.Lazy as LB
import Data.Aeson (decode)
import Api (Message(Message))
import Data.Text(Text)
import qualified Data.Text as T
import Network.URI.Encode (encode, encodeText)
import Data.Int (Int64, Int32)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Security (token, chatID)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple
    ( execute,
      query,
      close,
      connectPostgreSQL,
      Only(Only),
      Connection )
import Control.Exception (bracket)
import TextShow (TextShow(showt))
import Data.Text.IO (putStrLn)
import Prelude hiding (putStr, putStrLn, log)
import Control.Monad (replicateM_)
import Data.Functor ((<&>))



-- удобна не редачить сигнатуру для функций
data Env = Env {
  chat_id :: Int64, 
  response_text :: Text,
  send :: Env -> Text
  }

-- базовая сылка телеги
stringTelegram :: Text
stringTelegram = T.append "https://api.telegram.org/bot" token

-- реквест на отправку текста
sendText :: Env -> Text
sendText env@Env{..} = T.concat [
  stringTelegram,
  "/sendMessage?chat_id=", showt chat_id,
  "&text=", response_text]

-- реквест на отправку стикера
sendSticker :: Env -> Text
sendSticker env@Env{..} = T.concat [
  stringTelegram,
  "/sendSticker?chat_id=", showt chat_id,
  "&sticker=", response_text]




-- затычка для всех ответов на прямую
defult :: Response
defult = responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"

-- резльтат обработки пользовательских действий
data Message = 
    Stick Text -- для стикеров
  | Mes Text   -- для всех текстовых сообщений не являющимися командами
  | Repeat Int -- для /repeat n 
  | Start      -- для /start

--Api.text
-- обработка пользоватских действий
whichMessage :: Maybe Api.Message -> Lib.Message
whichMessage (Just mes) = case Api.text mes of
  Just txt -> whichText txt
  Nothing -> case Api.sticker mes of 
    Just st -> Stick $ Api.sticker_file_id st
    Nothing -> Mes "unsupported because this is not a text or sticker"
whichMessage Nothing = Mes "unsupported because this is not a message"

whichText :: Text -> Lib.Message
whichText "/start" = Start
whichText "/help" = Mes "my bot for metalamp"
whichText text = 
  if T.take 7 text == "/repeat" then let 
    text_num  = (T.drop 7 text) 
    maybe_num = readMaybe (T.unpack text_num) :: (Maybe Int)
    num       = fromMaybe 1 maybe_num
    in Repeat num
  else
    Mes text

-- существует ли уже такой юзер
isAssigned :: Connection -> Int64 -> IO Bool
isAssigned conn id = query conn select (Only id) <&> not . null @_ @(Only Int64)
  where select = "SELECT id FROM users WHERE id = ?"

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
updateRepeat id count = bracket (connectPostgreSQL connString) close (updateCount id count)
  where
    connString = "host=localhost dbname=metalamp"

-- обращается в базу за значением юзера
queryCount :: Int64 -> Connection -> IO Int
queryCount id conn = do
  ls <- query @_ @(Only Int) conn select (Only id)
  pure $ case ls of 
    [(Only n)] -> n 
    _          -> 1
  where
    select = "SELECT count FROM users WHERE id = ?"

-- запрос к базе metalamp queryCount обернытый в bracket на случай ошибки
queryText :: Int64 -> IO Int
queryText id = bracket (connectPostgreSQL connString) close (queryCount id)
  where
    connString = "host=localhost dbname=metalamp"

-- отправлет один раз сообщение
sendMessageOne :: Env -> IO ()
sendMessageOne env@Env{..} = do
  putStrLn response_text
  resp <- parseRequest (T.unpack $ send env) >>= httpLBS
  print resp
  return ()

-- 
sendMessage :: Env -> IO ()
sendMessage env@Env{..} = do
  n <- queryText chat_id
  replicateM_ n (sendMessageOne env)
  print response_text

chadId :: Api.Update -> Maybe Int64
chadId = fmap (Api.chat_id . Api.chat) . Api.message

startServer :: Maybe Api.Update -> IO ()
startServer (Just update) = do 
  print (show update)
  case chadId update of
    Nothing -> putStrLn "this not a message and what i need to do???" 
    Just chatid -> let 
      mes = whichMessage $ Api.message update
      in case mes of 
        Stick st -> sendMessage $ Env chatid st sendSticker
        Mes txt  -> sendMessage $ Env chatid txt sendText
        Repeat n -> updateRepeat chatid n
        Start    -> newRepeat chatid
startServer Nothing = putStrLn "aaaaaa something go wrong"

app :: Application
app req respond = trace (show req) $ case requestMethod req of
  "POST" -> do
    getRequestBodyChunk req >>= (startServer . decode @(Api.Update) . fromStrict)
    {- trace "this is POST "  $ -} 
    respond defult
  _     ->
    {- trace "this not POST " $ -} 
    respond defult

someFunc :: IO ()
someFunc = do
  run 8443 app






