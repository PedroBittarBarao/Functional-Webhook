-- DB.hs
{-# LANGUAGE OverloadedStrings #-}

module DB (initDB, hasTransaction, saveTransaction) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Text (Text)

initDB :: Connection -> IO ()
initDB conn = execute_ conn
  "CREATE TABLE IF NOT EXISTS transactions (transaction_id TEXT PRIMARY KEY)"

hasTransaction :: Connection -> Text -> IO Bool
hasTransaction conn txId = do
  res <- query conn "SELECT transaction_id FROM transactions WHERE transaction_id = ?" (Only txId) :: IO [Only Text]
  return $ not (null res)

saveTransaction :: Connection -> Text -> IO ()
saveTransaction conn txId =
  execute conn "INSERT INTO transactions (transaction_id) VALUES (?)" (Only txId)
