{-# LANGUAGE OverloadedStrings #-}
module RequestHandler (app) where

import Network.Wai (Application, Request, Response, responseLBS, requestMethod, strictRequestBody)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status400)
import Data.Aeson (decode, Value)
import qualified Data.ByteString.Lazy.Char8 as BL

import WebhookEvent (WebhookEvent)
import Data.Aeson (decode)

app :: Application
app req respond = do
  body <- strictRequestBody req
  putStrLn "Received raw body:"
  BL.putStrLn body

  case decode body :: Maybe WebhookEvent of
    Just event -> do
      putStrLn "Parsed event:"
      print event
      respond $ responseLBS status200 [("Content-Type", "text/plain")] "Webhook received"
    Nothing -> do
      putStrLn "Failed to parse WebhookEvent"
      respond $ responseLBS status400 [("Content-Type", "text/plain")] "Invalid JSON"



