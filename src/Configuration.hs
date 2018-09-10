{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Configuration where

import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           GHC.Generics

import           Api
import           Group
import           Light
import           Parsers
import           Scene
import           Types

data Config = Config {
  _name            :: String,
  zigbeechannel    :: Int,
  bridgeid         :: String,
  mac              :: String,
  dhcp             :: Bool,
  ipaddress        :: IPAddress,
  netmask          :: IPAddress,
  gateway          :: IPAddress,
  proxyport        :: Int,
  _UTC             :: ISO8601Time,
  localtime        :: ISO8601Time,
  timezone         :: String,
  modelid          :: String,
  datastoreversion :: String,
  swversion        :: String,
  apiversion       :: String,
  swupdate         :: SWUpdate,
  swupdate2        :: SWUpdate2,
  linkbutton       :: Bool,
  portalservices   :: Bool,
  portalconnection :: String,
  portalstate      :: PortalState,
  internetservices :: InternetServices,
  factorynew       :: Bool,
  replacesbridgeid :: Maybe String,
  backup           :: Backup,
  starterkitid     :: String,
  whitelist        :: Whitelist
} deriving (Show, Generic)

instance FromJSON Config where
  parseJSON = parseJSONIgnoreUnderscore

data Whitelist = Whitelist (HM.HashMap Username WhitelistEntry)
  deriving (Show, Generic)

instance FromJSON Whitelist

data WhitelistEntry = WhitelistEntry {
  lastUseDate :: ISO8601Time,
  createDate  :: ISO8601Time,
  name        :: String
} deriving (Show, Generic)

instance FromJSON WhitelistEntry where
  parseJSON = parseJSONCamelToWords

-- Get configuration

getConfigAPI :: IPAddress -> Username -> IO Config
getConfigAPI ip username = getAPI (authApiURL ip username ++ "/config")

getWhitelist :: IPAddress -> Username -> IO Whitelist
getWhitelist ip username = whitelist <$> getConfigAPI ip username

-- Modify configuration

modifyConfigAPI :: IPAddress -> Username -> Value -> IO [ApiResponse Value]
modifyConfigAPI ip username = putAPI (authApiURL ip username ++ "/config")

-- "Press" the link button

pressLinkButton :: IPAddress -> Username -> IO ()
pressLinkButton ip username =
  void $ modifyConfigAPI ip username (object ["linkbutton" .= True])

-- Get full datastore

data Datastore = Datastore {
  lights        :: ResourceMap Light,
  groups        :: ResourceMap Group,
  config        :: Config,
  schedules     :: ResourceMap Schedule,
  scenes        :: ResourceMap Scene,
  rules         :: ResourceMap Rule,
  sensors       :: ResourceMap Sensor,
  resourcelinks :: ResourceMap ResourceLink
} deriving (Show, Generic)

instance FromJSON Datastore

getDatastoreAPI :: IPAddress -> Username -> IO Datastore
getDatastoreAPI ip username = getAPI (authApiURL ip username)
