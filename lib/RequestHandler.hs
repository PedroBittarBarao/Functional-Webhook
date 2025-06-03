{-# LANGUAGE OverloadedStrings #-}
module RequestHandler (app) where

import Network.Wai (Application, Request, Response, responseLBS, strictRequestBody)
import Network.HTTP.Types (status200)
import Data.Aeson (decode, encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Exception (try, SomeException)
import Data.Text (Text)

import WebhookEvent (WebhookEvent(..))
import ConfirmTransaction (confirmTransaction)

app :: Application
app req respond = do
  body <- strictRequestBody req
  putStrLn "Received raw body:"
  BL.putStrLn body

  case decode body :: Maybe WebhookEvent of
    Just event -> do
      putStrLn "Parsed event:"
      print event
      let txId = transaction_id event
      
      -- Tenta enviar a confirmação e captura exceções para não quebrar o servidor
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
        status200
        [("Content-Type", "application/json")]
        (encode $ object ["error" .= ("invalid payload" :: Text)])
