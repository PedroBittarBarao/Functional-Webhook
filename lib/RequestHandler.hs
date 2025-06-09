{-# LANGUAGE OverloadedStrings #-}

module RequestHandler (app) where

import Network.Wai (Application, Request, Response, responseLBS, requestMethod, strictRequestBody, rawPathInfo, requestHeaders)
import Network.HTTP.Types (status200, status400, status403,status422)
import Data.Aeson (decode, encode, object, (.=),Value(..),Object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import Data.IORef
import qualified Data.Set as Set
import Database.SQLite.Simple (Connection)

import DB (initDB, hasTransaction, saveTransaction)
import WebhookEvent (WebhookEvent(..))
import ConfirmTransaction (confirmTransaction)
import CancelTransaction (cancelTransaction)

expectedToken :: Text
expectedToken = "meu-token-secreto"

app :: Connection -> Application
app conn req respond = do
  let path = rawPathInfo req
  if path /= "/webhook"
    then respond $ responseLBS status400 [("Content-Type", "application/json")] "{\"error\": \"Not found\"}"
    else do
      body <- strictRequestBody req
      putStrLn "Received raw body:"
      BL.putStrLn body

      case decode body :: Maybe WebhookEvent of
        Nothing -> do
          putStrLn "Failed to parse WebhookEvent"
          case decode body :: Maybe (Object) of
            Just o -> case KM.lookup "transaction_id" o of
              Just (Aeson.String txId) -> do
                putStrLn $ "Invalid event payload, but found transaction_id: " ++ show txId
                cancelTransaction txId
                respond $ responseLBS status400 [("Content-Type", "application/json")]
                  (encode $ object ["error" .= ("Invalid JSON, transaction canceled" :: Text), "transaction_id" .= txId])
              _ -> do
                putStrLn "Invalid JSON and no transaction_id found"
                respond $ responseLBS status400 [("Content-Type", "application/json")]
                  "{\"error\": \"Invalid JSON payload\"}"
            Nothing -> do
              putStrLn "Completely invalid JSON"
              respond $ responseLBS status400 [("Content-Type", "application/json")]
                "{\"error\": \"Invalid JSON payload\"}"
          
        Just ev -> do
          putStrLn "Parsed event:"
          print ev

          let headers = requestHeaders req
              token = fmap TE.decodeUtf8 (lookup "X-Webhook-Token" headers)

          if token /= Just expectedToken
            then do
              putStrLn "Invalid token, ignoring transaction"
              respond $ responseLBS status403 [("Content-Type", "application/json")] "{\"error\": \"Invalid token\"}"
            else do
              -- Verifica se algum campo obrigatório (exceto transaction_id) está vazio
              let missingFields = any (== "") [currency ev, timestamp ev, event ev]

              if missingFields
                then do
                  putStrLn "Missing fields, canceling transaction"
                  cancelTransaction (transaction_id ev)
                  respond $ responseLBS status200 [("Content-Type", "application/json")]
                    (encode $ object ["transaction_id" .= transaction_id ev])
                else do
                  alreadyProcessed <- hasTransaction conn (transaction_id ev)
                  if alreadyProcessed
                    then do
                      putStrLn "Duplicate transaction"
                      respond $ responseLBS status400 [("Content-Type", "application/json")]
                        "{\"error\": \"Duplicate transaction\"}"
                    else do
                      saveTransaction conn (transaction_id ev)
                      -- Continue processing


                      if amount ev == 0
                        then do
                          putStrLn "Amount is 0, canceling transaction"
                          cancelTransaction (transaction_id ev)
                          respond $ responseLBS status422 [("Content-Type", "application/json")]
                            (encode $ object ["error" .= ("Amount is zero, transaction canceled" :: Text), "transaction_id" .= transaction_id ev])
                        else do
                          putStrLn "Confirming transaction"
                          confirmTransaction (transaction_id ev)
                          respond $ responseLBS status200 [("Content-Type", "application/json")]
                            (encode $ object ["transaction_id" .= transaction_id ev])

                      respond $ responseLBS status200 [("Content-Type", "application/json")]
                        (encode $ object ["transaction_id" .= transaction_id ev])
