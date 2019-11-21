{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Group where

import           Data.Aeson
import           Data.Aeson.Types
import           GHC.Generics

import           Api
import           LightState
import           Parsers
import           Types

allLights :: ResourceID
allLights = "0"

data Group = Group {
  name      :: String,
  lights    :: [ResourceID],
  _type     :: GroupType,
  state     :: GroupState,
  recycle   :: Bool,
  _class    :: Maybe String,
  stream    :: Maybe Stream,
  locations :: Maybe Locations,
  action    :: LightState
} deriving (Show, Generic)

instance FromJSON Group where
  parseJSON = parseJSONIgnoreUnderscore

data GroupType = Luminaire | Lightsource | LightGroup | Room | Entertainment
  deriving (Show, Generic)

instance FromJSON GroupType

data GroupState = GroupState {
  allOn :: Bool,
  anyOn :: Bool
} deriving (Show, Generic)

instance FromJSON GroupState where
  parseJSON = parseJSONCamelToSnakeCase

type Locations = ResourceMap (Double, Double, Double)

-- Get groups

getGroupsAPI :: IPAddress -> Username -> IO (ResourceMap Group)
getGroupsAPI ip username = getAPI (authApiURL ip username ++ "/groups")

getGroupAPI :: IPAddress -> Username -> ResourceID -> IO Group
getGroupAPI ip username groupID =
  getAPI (authApiURL ip username ++ "/groups/" ++ groupID)

-- Set group state

setGroupStateAPI :: IPAddress -> Username -> ResourceID -> LightStateParam -> IO [ApiResponse Value]
setGroupStateAPI ip username groupID =
  putAPI (authApiURL ip username ++ "/groups/" ++ groupID ++ "/action") . object . unLightState

setScene :: String -> Pair
setScene = ("scene" .=)
