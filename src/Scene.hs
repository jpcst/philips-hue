{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Scene where

import           Control.Monad
import           Data.Aeson
import           Data.Char
import qualified Data.HashMap.Strict as HM
import           GHC.Generics

import           Api
import           Group               hiding (name)
import           Parsers
import           Types

data Scene = Scene {
  name        :: String,
  lights      :: [ResourceID],
  owner       :: String,
  recycle     :: Bool,
  locked      :: Bool,
  appdata     :: AppData,
  picture     :: String,
  lastupdated :: ISO8601Time,
  version     :: Int,
  -- Only present when fetching individual scenes
  lightstates :: Maybe (ResourceMap SceneLightState)
} deriving (Show, Generic)

instance FromJSON Scene

data AppData = AppData {
  _version :: Maybe Int,
  _data    :: Maybe String
} deriving (Show, Generic)

instance FromJSON AppData where
  parseJSON = parseJSONIgnoreUnderscore

data SceneLightState = SceneLightState {
  xy             :: Maybe (Double, Double),
  hue            :: Maybe Int,
  sat            :: Maybe Int,
  ct             :: Maybe Int,
  bri            :: Maybe Int,
  on             :: Maybe Bool,
  effect         :: Maybe String,
  transitiontime :: Maybe Int
} deriving (Show, Generic)

instance FromJSON SceneLightState

-- Get scenes

getScenesAPI :: IPAddress -> Username -> IO (ResourceMap Scene)
getScenesAPI ip username = getAPI (authApiURL ip username ++ "/scenes")

getSceneAPI :: IPAddress -> Username -> ResourceID -> IO Scene
getSceneAPI ip username sceneID =
  getAPI (authApiURL ip username ++ "/scenes/" ++ sceneID)

-- Recall scene

recallScene :: IPAddress -> Username -> ResourceID -> IO ()
recallScene ip username sceneID = do
  response <- setGroupStateAPI ip username allLights (object [setScene sceneID])
  case response of
    [ApiError 7 _ desc] -> putStrLn desc
    _ -> return ()

recallSceneByName :: IPAddress -> Username -> String -> IO ()
recallSceneByName ip username sceneName = do
  scenes <- getScenesAPI ip username
  let sceneIDs = [ sceneID | (sceneID, scene) <- HM.toList scenes,
                             toLowercase (name scene) == toLowercase sceneName]
  if null sceneIDs
    then putStrLn $ "No scenes found with name \"" ++ sceneName ++ "\""
    else forM_ sceneIDs (recallScene ip username)

  where toLowercase = map toLower
