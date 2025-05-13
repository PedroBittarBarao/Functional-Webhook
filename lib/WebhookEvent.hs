{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module WebhookEvent (WebhookEvent) where

import Data.Fixed (Fixed, E2)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Fixed (Fixed, E2)


data WebhookEvent = WebhookEvent {
    event          :: String   ,
    transaction_id :: String   ,
    amount         :: Fixed E2 ,
    currency       :: String   ,
    timestamp      :: String
} deriving (Show, Generic)

instance FromJSON WebhookEvent
instance ToJSON WebhookEvent