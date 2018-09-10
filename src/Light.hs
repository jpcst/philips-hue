{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Light where

import           Data.Aeson
import           GHC.Generics

import           Api
import           LightState
import           Parsers
import           Types

data Light = Light {
  state            :: LightState,
  swupdate         :: LightSWUpdate,
  _type            :: String,
  name             :: String,
  modelid          :: String,
  manufacturername :: String,
  productname      :: String,
  capabilities     :: Capabilities,
  config           :: LightConfig,
  uniqueid         :: String,
  swversion        :: String,
  swconfigid       :: String,
  productid        :: String
} deriving (Show, Generic)

instance FromJSON Light where
  parseJSON = parseJSONIgnoreUnderscore

-- Get lights

getLightsAPI :: IPAddress -> Username -> IO (ResourceMap Light)
getLightsAPI ip username = getAPI (authApiURL ip username ++ "/lights")

getLightAPI :: IPAddress -> Username -> ResourceID -> IO Light
getLightAPI ip username lightID =
  getAPI (authApiURL ip username ++ "/lights/" ++ lightID)
