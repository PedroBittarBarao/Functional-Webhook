{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module WebhookEvent where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), ToJSON, (.:), withObject)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Fixed (Fixed, E2)
import qualified Data.Text.Read as TR

data WebhookEvent = WebhookEvent
  { event          :: Text
  , transaction_id :: Text
  , amount         :: Fixed E2
  , currency       :: Text
  , timestamp      :: Text
  } deriving (Show, Generic)

instance ToJSON WebhookEvent

instance FromJSON WebhookEvent where
  parseJSON = withObject "WebhookEvent" $ \v -> do
    event <- v .: "event"
    transaction_id <- v .: "transaction_id"
    amountStr <- v .: "amount" :: Parser Text
    amount <- case TR.rational amountStr of
      Right (n, _) -> pure (realToFrac n)
      Left err -> fail $ "Invalid amount format: " ++ err
    currency <- v .: "currency"
    timestamp <- v .: "timestamp"
    return WebhookEvent
      { event = event
      , transaction_id = transaction_id
      , amount = amount
      , currency = currency
      , timestamp = timestamp
      }
