{-# LANGUAGE OverloadedStrings #-}

module RequestHandler (app) where

import Network.Wai (Application, Request, Response, responseLBS, requestMethod, strictRequestBody, rawPathInfo)
import Network.HTTP.Types (status200, status400)
import Data.Aeson (decode, encode, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as Set
import Data.IORef
import Data.Text (Text)
import WebhookEvent (WebhookEvent(..))
import ConfirmTransaction (confirmTransaction)
import CancelTransaction (cancelTransaction)

app :: IORef (Set.Set Text) -> Application
app processedRef req respond = do
  let path = rawPathInfo req
  if path == "/webhook"
    then do
      body <- strictRequestBody req
      putStrLn "Received raw body:"
      BL.putStrLn body

      case decode body :: Maybe WebhookEvent of
        Just event -> do
          putStrLn "Parsed event:"
          print event
          alreadyProcessed <- Set.member (transaction_id event) <$> readIORef processedRef
          if alreadyProcessed
            then do
              putStrLn "Duplicated transaction"
              respond $ responseLBS status400 [("Content-Type", "application/json")] (encode $ object ["error" .= ("duplicated transaction" :: Text)])
            else do
              modifyIORef processedRef (Set.insert (transaction_id event))
              if amount event == 0.00
                then cancelTransaction (transaction_id event)
                else confirmTransaction (transaction_id event)
              respond $ responseLBS status200 [("Content-Type", "application/json")] (encode $ object ["transaction_id" .= transaction_id event])
        Nothing -> do
          putStrLn "Failed to parse WebhookEvent"
          respond $ responseLBS status400 [("Content-Type", "application/json")] (encode $ object ["error" .= ("invalid payload" :: Text)])
    else
      respond $ responseLBS status400 [("Content-Type", "application/json")] (encode $ object ["error" .= ("invalid route" :: Text)])
