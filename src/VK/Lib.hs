module VK.Lib
  ( startServer,
  )
where

import Config (Settings (..))
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
import Data.Aeson (eitherDecode)
import Data.Aeson.Types (FromJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy as LB
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO (putStrLn)
import GHC.Generics (Generic (..))
import Network.HTTP.Client
  ( Request,
    Response (..),
    httpLbs,
    parseRequest,
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
    sendQuery,
    setLongPollSettings,
  )
import VK.Types (ChatId, Env (..), MessageRequest (..), PeerId, RandomId, UserId)
import Prelude hiding (putStrLn)

{-
send to vk request
-}
post :: Text -> IO (Response LB.ByteString)
post request = do
  manager <- getGlobalManager
  resp <- parseRequest (T.unpack request)
  httpLbs resp manager

{-
need function to constract request and after send it
-}
sendOne :: (RandomId -> Request -> Request) -> IO ()
sendOne send = do
  randomId <- randomIO
  manager <- getGlobalManager
  initRequest <- parseRequest (T.unpack sendQuery)
  let request = send randomId initRequest
  print =<< httpLbs request manager

{-
send reqest n times
-}
sendN :: Table -> UserId -> (RandomId -> Request -> Request) -> IO ()
sendN table userId send = do
  n <- queryRepeat table userId
  replicateM_ n (sendOne send)
  print "end sendN"

{-
if payload come with Message he can have start command or buttons
if Command start then newRepeat
if Button num then updateRepeat
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
user send message and we react somehow
-}
whichText :: Text -> MessageRequest
whichText "/help" = Help
whichText "/repeat" = Repeat
whichText text = Mes text

{-
after processing message we need to do something
1) if text then send text n times
2) if /help then send text from config
3) if /repeat then send keyboard and question from config
-}
action :: Env -> UserId -> PeerId -> MessageRequest -> IO ()
action env@Env {..} userId peerId mes = do
  let sendIO = sendN table userId
  case mes of
    Mes txt -> do
      print "wow message!!!!"
      let send = requestMessage env userId txt
      void $ sendIO send
    Help -> do
      print "its help"
      let send = requestMessage env userId help
      void $ sendIO send
    Repeat -> do
      print "its repeat"
      let send = requestKeyBoard env userId peerId
      let sendQuestion = requestMessage env userId repeat
      void $ sendIO sendQuestion
      void $ sendOne send

{-
now we have update and do something with it
check if the field paylod exists
if paylod have start then create user in db
if paylod have button then update value on db
check if the field attachments is not empty
if update unknown then do nothing :shrag:
-}
actionUpdate :: Env -> Update -> IO ()
actionUpdate env@Env {..} (Update (Object message)) = do
  let Message userId attachments peerId text payload = message
  print "well update"
  void $ actionCommand env payload userId
  case attachments of
    [] -> do
      print "i think is message"
      action env userId peerId $ whichText text
    ats -> mapM_ sendSt ats
      where
        sendSt (Attachments (Sticker stickerId)) = do
          print "wow sticker!!!!"
          let send = requestSticker env userId peerId stickerId
          void $ sendN table userId send

{-
send request and decode it, throw error, because we can't start
-}
requestDecode :: FromJSON a => Text -> IO a
requestDecode request = do
  resp <- post request
  case eitherDecode (responseBody resp) of
    Right a -> pure a
    _ -> error "unknown vk response"

{-
every 3 second send to vk request and update offset after it
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
with config and settings start vk bot
-}
startServer :: Config -> Settings -> IO ()
startServer Config {..} Settings {..} = do
  print "start"
  let table = fromString tableString
  VKResponse lps <- requestDecode $ getLongPollServer token groupId
  print "well done"
  set_resp <- post $ setLongPollSettings token groupId
  print set_resp
  let env = Env {..}
  handleUpdate env (actionUpdate env)
