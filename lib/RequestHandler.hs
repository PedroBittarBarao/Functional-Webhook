{-# LANGUAGE OverloadedStrings #-}

module RequestHandler (app) where

import Network.Wai (Application, Request, Response, responseLBS, strictRequestBody)
import Network.HTTP.Types (status200, status400)
import Data.Aeson (decode, encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import Data.IORef
import qualified Data.Set as Set
import Control.Exception (try, SomeException)

import WebhookEvent (WebhookEvent(..))
import ConfirmTransaction (confirmTransaction)

app :: IORef (Set.Set Text) -> Application
app processedRef req respond = do
  body <- strictRequestBody req
  putStrLn "Received raw body:"
  BL.putStrLn body

  case decode body :: Maybe WebhookEvent of
    Just event -> do
      let txId = transaction_id event
      alreadyProcessed <- Set.member txId <$> readIORef processedRef

      if alreadyProcessed
        then do
          putStrLn $ "Transação já processada: " ++ show txId
          respond $ responseLBS
            status400
            [("Content-Type", "application/json")]
            (encode $ object ["error" .= ("duplicated transaction" :: Text)])
        else do
          putStrLn "Parsed event:"
          print event
          modifyIORef' processedRef (Set.insert txId)

          result <- try (confirmTransaction txId) :: IO (Either SomeException ())
          case result of
            Left err -> putStrLn $ "Erro ao confirmar transação: " ++ show err
            Right _  -> putStrLn "Confirmação enviada com sucesso"

          respond $ responseLBS
            status200
            [("Content-Type", "application/json")]
            (encode $ object ["transaction_id" .= txId])

    Nothing -> do
      putStrLn "Failed to parse WebhookEvent"
      respond $ responseLBS
        status400
        [("Content-Type", "application/json")]
        (encode $ object ["error" .= ("invalid payload" :: Text)])
