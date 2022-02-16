{-# LANGUAGE TypeApplications #-}
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
import Data.Int (Int64)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Security (token, chatID)



-- удобна :)
data Env = Env {
  chat_id :: Int64, 
  response_text :: Text
  }

-- базовая сылка телеги
stringTelegram :: Text
stringTelegram = T.append "https://api.telegram.org/bot" token

tshow :: Show a => a -> Text
tshow = T.pack . show

-- реквест нормального человека
stringRequest :: Env -> Text
stringRequest env@Env{..} = T.concat [
  stringTelegram,
  "/sendMessage?chat_id=", tshow chat_id,
  "&text=", response_text]






defult :: Response
defult = responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"

help :: Text
help = "my bot for metalamp"

defultMessage :: Text 
defultMessage = "can't undestand your message"

whichText :: Maybe Text -> Text
whichText (Just "/help") = help
whichText (Just text) = 
  if T.take 7 text == "/repeat" then let 
    text_num  = (T.drop 7 text) 
    maybe_num = readMaybe (T.unpack text_num) :: (Maybe Int)
    num       = fromMaybe 0 maybe_num
    in T.replicate num text
  else 
    defultMessage
whichText _ = defultMessage

sendText :: Env -> IO ()
sendText env@Env{..} = do
  print response_text
  resp <- parseRequest (T.unpack $ stringRequest env) >>= httpLBS
  print resp
  return ()

updateToMessage :: Api.Update -> Int64
updateToMessage update = case Api.message update of 
  Nothing -> chatID -- undefined
  Just mes -> Api.chat_id $ Api.chat mes

startServer :: Maybe Api.Update -> IO ()
startServer (Just update) =
  sendText $ 
    Env (updateToMessage update) 
        (whichText $ maybe (Just "no message :(") 
                           Api.text 
                           (Api.message update))
startServer Nothing = trace "not get Update :(" $ print "aaaaaa something go wrong"

app :: Application
app req respond = {- trace (show req) $ -} case requestMethod req of
  "POST" -> do
    getRequestBodyChunk req >>= (startServer . decode @(Api.Update) . fromStrict)
    {- trace "this is POST "  $ -} 
    respond defult
  _     ->
    {- trace "this not POST " $ -} 
    respond defult

someFunc :: IO ()
someFunc = run 8443 app






