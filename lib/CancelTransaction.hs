{-# LANGUAGE OverloadedStrings #-}

module CancelTransaction (cancelTransaction) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Aeson as Aeson
import Data.Text (Text)

cancelTransaction :: Text -> IO ()
cancelTransaction txId = do
  manager <- newManager tlsManagerSettings
  let url = "http://localhost:5001/cancelar"
      payload = Aeson.object ["transaction_id" Aeson..= txId]
      requestBody = RequestBodyLBS (Aeson.encode payload)
  initReq <- parseRequest url
  let request = initReq
        { method = "POST"
        , requestBody = requestBody
        , requestHeaders = [("Content-Type", "application/json")]
        }
  response <- httpLbs request manager
  putStrLn $ "Sent cancelation request: " ++ show (responseStatus response)
