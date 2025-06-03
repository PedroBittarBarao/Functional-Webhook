{-# LANGUAGE OverloadedStrings #-}

module ConfirmTransaction (confirmTransaction) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Aeson as Aeson
import Data.Text (Text)

confirmTransaction :: Text -> IO ()
confirmTransaction txId = do
  manager <- newManager tlsManagerSettings
  let url = "http://localhost:5001/confirmar"
      payload = Aeson.object ["transaction_id" Aeson..= txId]
      requestBody = RequestBodyLBS (Aeson.encode payload)
  initReq <- parseRequest url
  let request = initReq
        { method = "POST"
        , requestBody = requestBody
        , requestHeaders = [("Content-Type", "application/json")]
        }
  response <- httpLbs request manager
  putStrLn $ "Enviada confirmação: " ++ show (responseStatus response)
