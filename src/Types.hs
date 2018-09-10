{-# LANGUAGE OverloadedStrings #-}
module Types where

import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM

type IPAddress = String
type ResourceID = String
type URL = String
type Username = String

type ResourceMap a = HM.HashMap ResourceID a

-- TODO: Add real types for these
type Backup = Object
type Capabilities = Object
type InternetServices = Object
type LightConfig = Object
type LightSWUpdate = Object
type PortalState = Object
type ResourceLink = Object
type Rule = Object
type Schedule = Object
type Sensor = Object
type Stream = Object
type SWUpdate = Object
type SWUpdate2 = Object
