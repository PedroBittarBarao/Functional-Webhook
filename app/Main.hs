{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Database.SQLite.Simple (open)
import DB (initDB)
import RequestHandler (app)

main :: IO ()
main = do
  conn <- open "transactions.db"
  initDB conn
  putStrLn "Listening on port 5001..."
  run 5000 (app conn)
