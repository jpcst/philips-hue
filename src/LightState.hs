{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module LightState where

import           Data.Aeson.Types
import           GHC.Generics
import           System.Random

import           Api
import           Types

data LightState = LightState {
  on        :: Bool,
  bri       :: Int,
  hue       :: Int,
  sat       :: Int,
  effect    :: String,
  xy        :: (Double, Double),
  ct        :: Int,
  alert     :: String,
  colormode :: String,
  mode      :: Maybe String,
  reachable :: Maybe Bool
} deriving (Show, Generic)

instance FromJSON LightState

-- A JSON value that can be used to specify a light state
newtype LightStateParam = LightStateParam {unLightState :: [Pair]}

setOn :: Bool -> Pair
setOn = ("on" .=)

setBri :: Int -> Pair
setBri = ("bri" .=)

setHue :: Int -> Pair
setHue = ("hue" .=)

setSat :: Int -> Pair
setSat = ("sat" .=)

setXY :: (Double, Double) -> Pair
setXY (x,y) = "xy" .= [x,y]

setCT :: Int -> Pair
setCT = ("ct" .=)

setAlert :: String -> Pair
setAlert = ("alert" .=)

setEffect :: String -> Pair
setEffect = ("effect" .=)

setTransitionTime :: Int -> Pair
setTransitionTime = ("transitiontime" .=)

setBriInc :: Int -> Pair
setBriInc = ("bri_inc" .=)

setSatInc :: Int -> Pair
setSatInc = ("sat_inc" .=)

setHueInc :: Int -> Pair
setHueInc = ("hue_inc" .=)

setCTInc :: Int -> Pair
setCTInc = ("ct_inc" .=)

setXYInc :: (Double, Double) -> Pair
setXYInc = ("xy_inc" .=)

-- Set light state

setLightStateAPI :: IPAddress -> Username -> ResourceID -> LightStateParam -> IO [ApiResponse Value]
setLightStateAPI ip username lightID =
  putAPI (authApiURL ip username ++ "/lights/" ++ lightID ++ "/state") . object . unLightState

instance Random LightStateParam where
  randomR _ = random
  random g = (LightStateParam value, g''')
    where value = [setBri bri', setHue hue', setSat sat']
          (bri', g') = randomR (0, 254) g
          (hue', g'') = randomR (0, 65535) g'
          (sat', g''') = randomR (0, 254) g''
