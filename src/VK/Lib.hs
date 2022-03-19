module VK.Lib
  ( startServer,
  )
where

import Config (Settings (..), Database (..))
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
import PostgresQuery (
  Table, 
  Postgres(..),
  newRepeat, 
  queryRepeat, 
  updateRepeat)
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
    setLongPollSettings,
  )
import VK.Types (ChatId, Env (..), MessageRequest (..), PeerId, RandomId, UserId)
import Prelude hiding (putStrLn)
import Logger (Log(..), logFunctions)

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
sendOne :: Env -> (RandomId -> Request -> Request) -> IO ()
sendOne Env {..} send = do
  logInfo "start SendOne"
  randomId <- randomIO
  manager <- getGlobalManager
  let query = api `T.append` "/messages.send"
  initRequest <- parseRequest (T.unpack query)
  let request = send randomId initRequest
  logInfo $ "request:" ++ show request
  resp <- httpLbs request manager
  logDebug $ "response:" ++ show resp
  logInfo "end sendOne"

{-
send reqest n times
-}
sendN :: Env -> UserId -> (RandomId -> Request -> Request) -> IO ()
sendN env@Env {..} userId send = do
  n <- queryRepeat Postgres{..} userId
  replicateM_ n (sendOne env send)

{-
if payload come with Message he can have start command or buttons
if Command start then newRepeat
if Button num then updateRepeat
-}
actionCommand :: Env -> Maybe Text -> ChatId -> IO ()
actionCommand Env {..} payload userId = case payload of
  Nothing -> logInfo "no payload"
  Just txt -> case eitherDecode (fromStrict . encodeUtf8 $ txt) of
    Left s1 -> case eitherDecode @Command (fromStrict . encodeUtf8 $ txt) of
      Left s2 -> do 
        logWarn $ "it not a button:" ++ s1
        logWarn $ "it not a Command:" ++ s2
      Right com ->
        if command com == "start"
          then newRepeat Postgres{..} userId
          else logWarn "this command not a start???"
    Right (Button textNum) -> do
      let num = fromMaybe (1 :: Int) (readMaybe (T.unpack textNum))
      updateRepeat Postgres{..} userId num
      logInfo ("button: " ++ show num)

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
  let sendIO = sendN env userId
  case mes of
    Mes txt -> do
      logInfo "its message"
      let send = requestMessage env userId txt
      void $ sendIO send
    Help -> do
      logInfo "its help"
      let send = requestMessage env userId help
      void $ sendIO send
    Repeat -> do
      logInfo "its repeat"
      let send = requestKeyBoard env userId peerId
      let sendQuestion = requestMessage env userId repeat
      void $ sendIO sendQuestion
      void $ sendOne env send

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
  logInfo "startActionUpdate"
  let Message userId attachments peerId text payload = message
  void $ actionCommand env payload userId
  case attachments of
    [] -> do
      logInfo "maybe is message"
      action env userId peerId $ whichText text
    ats -> mapM_ sendSt ats
      where
        sendSt (Attachments (Sticker stickerId)) = do
          logInfo "it is a sticker"
          let send = requestSticker env userId peerId stickerId
          void $ sendN env userId send
  logInfo "endActionUpdate"

{-
send request and decode it, throw error, because we can't start
-}
requestDecode :: (Show a, FromJSON a) => Text -> IO a
requestDecode request = do
  resp <- post request
  case eitherDecode (responseBody resp) of
    Right a -> pure a
    _e -> error $ "unknown vk response:" ++ show _e

{-
every 3 second send to vk request and update offset after it
-}
handleUpdate :: Env -> (Update -> IO b) -> IO a
handleUpdate env@Env {..} action = do
  logInfo "start loop updates"
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
  let Database{..} = database
  let Log{..} = logFunctions loglevel
  logInfo "Start VK server"
  let table = fromString tableString
  VKResponse lps <- requestDecode $ getLongPollServer token api groupId
  logInfo "get vkresponcse"
  logDebug $ "VKResponse:" ++ show lps
  set_resp <- post $ setLongPollSettings token api groupId
  logDebug $ "vk response settings:" ++ show set_resp
  let env = Env {..}
  handleUpdate env (actionUpdate env)
