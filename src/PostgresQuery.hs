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

-- существует ли уже такой юзер
isAssigned :: Table -> Connection -> ChatId -> IO Bool
isAssigned users conn chatId =
  query @_ @(Only Int64) conn select (Only chatId) <&> not . null
  where
    select = "SELECT id FROM " <> users <> " WHERE id = ?"

-- увеличивает count для юзера
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

-- создает юзера или ставит ему значение 1
newRepeat :: Def -> Table -> ChatId -> IO ()
newRepeat def users chatId = updateRepeat users chatId def

-- создает юзера или ставит ему значение n
updateRepeat :: Table -> ChatId -> Int -> IO ()
updateRepeat users chatId n = bracket (connectPostgreSQL connString) close update
  where
    connString = "host=localhost dbname=metalamp"
    update = updateCount users chatId n

-- обращается в базу за значением юзера
queryCount :: Table -> ChatId -> Connection -> IO Int
queryCount users chatId conn = do
  ls <- query @_ @(Only Int) conn select (Only chatId)
  pure $ case ls of
    [(Only n)] -> n
    _ -> 1
  where
    select = "SELECT count FROM " <> users <> " WHERE id = ?"

-- запрос к базе metalamp queryCount обернытый в bracket на случай ошибки
queryRepeat :: Table -> ChatId -> IO Int
queryRepeat users = bracket (connectPostgreSQL connString) close . queryCount users
  where
    connString = "host=localhost dbname=metalamp"
