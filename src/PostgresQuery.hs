module PostgresQuery
  ( newRepeat,
    updateRepeat,
    queryRepeat,
    Table,
    Postgres (..),
  )
where

import Control.Exception (bracket)
import Control.Monad (void, unless)
import Data.Functor ((<&>))
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only),
    Query (..),
    close,
    connectPostgreSQL,
    execute,
    query,
  )

type Table = Query

type ChatId = Int64

type Def = Int

{-
Postgres enviroment
-}
data Postgres = Postgres
  { table :: Table,
    defaultRepeat :: Int,
    host :: Text,
    dbname :: Text,
    logDebug :: String -> IO (),
    logInfo :: String -> IO (),
    logWarn :: String -> IO ()
  }

{-
does this user already exist?
-}
isAssigned :: Table -> Connection -> ChatId -> IO Bool
isAssigned users conn chatId =
  query @_ @(Only Int64) conn select (Only chatId) <&> not . null
  where
    select = "SELECT id FROM " <> users <> " WHERE id = ?"

{-
increment count for user
-}
updateCount :: Postgres -> ChatId -> Int -> Connection -> IO ()
updateCount Postgres {..} chatId newCount conn = do
  bool <- isAssigned table conn chatId
  res <- case bool of
    True -> do
      let update = "UPDATE " <> table <> " set count = ? where id = ?"
      execute conn update (newCount, chatId)
    False -> do
      let insert = "INSERT INTO " <> table <> " (id, count) VALUES (?, ?)"
      execute conn insert (chatId, newCount)
  logDebug $ "database result:" ++ show res

{-
create user and set defualt repeat for it
-}
newRepeat :: Postgres -> ChatId -> IO ()
newRepeat post@Postgres {..} chatId = updateRepeat post chatId defaultRepeat

{-
bracket version updateCount
-}
updateRepeat :: Postgres -> ChatId -> Int -> IO ()
updateRepeat post@Postgres {..} chatId =
  bracket (connectPostgreSQL connString) close . (updateCount post chatId)
  where
    connString =
      encodeUtf8 $
        T.concat
          [ "host=" `T.append` host,
            " ",
            "dbname=" `T.append` dbname
          ]
{-
help function
-}
isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False

{-
get count value from user table
-}
queryCount :: Postgres -> ChatId -> Connection -> IO Int
queryCount Postgres {..} chatId conn = do
  ls <- query @_ @(Only Int) conn select (Only chatId)
  unless (isSingleton ls) $ logWarn $ "it can't be" ++ show ls
  pure $ case ls of
    [(Only n)] -> n
    _ -> 1
  where
    select = "SELECT count FROM " <> table <> " WHERE id = ?"

{-
bracket version queryCount
-}
queryRepeat :: Postgres -> ChatId -> IO Int
queryRepeat post@Postgres {..} =
  bracket (connectPostgreSQL connString) close . queryCount post
  where
    connString =
      encodeUtf8 $
        T.concat
          [ "host=" `T.append` host,
            " ",
            "dbname=" `T.append` dbname
          ]
