{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Bridge
import           Types
import           User

getSetup :: IO (IPAddress, Username)
getSetup = do
  ip <- getPrimaryBridgeIP
  username <- getUsername ip
  return (ip, username)
