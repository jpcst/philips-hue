{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Bridge where

import           Data.Aeson
import           GHC.Generics

import           Api
import           Types

data Bridge = Bridge {
  id                :: String,
  internalipaddress :: IPAddress
} deriving (Show, Generic)

instance FromJSON Bridge

-- Get bridges on the local network

getBridgesAPI :: IO [Bridge]
getBridgesAPI = getAPI "https://discovery.meethue.com"

-- Get the main bridge IP

getPrimaryBridgeIP :: IO IPAddress
getPrimaryBridgeIP = do
  (bridge:_) <- getBridgesAPI
  return $ internalipaddress bridge
