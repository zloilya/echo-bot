module PostgresQuery
  ( newRepeat,
    updateRepeat,
    queryRepeat,
    Table
  )
where

import Control.Exception (bracket)
import Data.Functor ((<&>))
import Data.Int (Int32, Int64)
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
updateCount :: Table -> ChatId -> Int -> Connection -> IO ()
updateCount users chatId newCount conn = do
  bool <- isAssigned users conn chatId
  print ("isAssigned: " ++ show bool)
  res <- case bool of
    True -> do
      let update = "UPDATE " <> users <> " set count = ? where id = ?"
      execute conn update (newCount, chatId)
    False -> do
      let insert = "INSERT INTO " <> users <> " (id, count) VALUES (?, ?)"
      execute conn insert (chatId, newCount)
  print ("execute: " ++ show res)

{-
create user and set defualt repeat for it
-}
newRepeat :: Def -> Table -> ChatId -> IO ()
newRepeat def users chatId = updateRepeat users chatId def

{-
bracket version updateCount
-}
updateRepeat :: Table -> ChatId -> Int -> IO ()
updateRepeat users chatId n = bracket (connectPostgreSQL connString) close update
  where
    connString = "host=localhost dbname=metalamp"
    update = updateCount users chatId n

{-
get count value from user table
-}
queryCount :: Table -> ChatId -> Connection -> IO Int
queryCount users chatId conn = do
  ls <- query @_ @(Only Int) conn select (Only chatId)
  pure $ case ls of
    [(Only n)] -> n
    _ -> 1
  where
    select = "SELECT count FROM " <> users <> " WHERE id = ?"

{-
bracket version queryCount
-}
queryRepeat :: Table -> ChatId -> IO Int
queryRepeat users = bracket (connectPostgreSQL connString) close . queryCount users
  where
    connString = "host=localhost dbname=metalamp"
