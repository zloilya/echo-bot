module PostgresQuery
  ( newRepeat,
    updateRepeat,
    queryText,
  )
where

import Control.Exception (bracket)
import Data.Functor ((<&>))
import Data.Int (Int32, Int64)
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only),
    close,
    connectPostgreSQL,
    execute,
    query,
  )

-- существует ли уже такой юзер
isAssigned :: Connection -> Int64 -> IO Bool
isAssigned conn id = query @_ @(Only Int64) conn select (Only id) <&> not . null
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
newRepeat chat_id = updateRepeat chat_id 1

-- создает юзера или ставит ему значение n
updateRepeat :: Int64 -> Int -> IO ()
updateRepeat chatid = ((bracket (connectPostgreSQL connString) close)) . updateCount chatid
  where
    connString = "host=localhost dbname=metalamp"

-- обращается в базу за значением юзера
queryCount :: Int64 -> Connection -> IO Int
queryCount chat_id conn = do
  ls <- query @_ @(Only Int) conn select (Only chat_id)
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
