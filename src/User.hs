{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module User where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           GHC.Generics

import           Api
import           Configuration
import           Constants
import           Parsers
import           Types

-- Get the current user, or create one if none exists
-- TODO: Verify that the current user is authenticated

getUsername :: IPAddress -> IO Username
getUsername ip = catch (readFile usernameFile) $ \(SomeException _) -> do
  username <- createUser ip
  writeFile usernameFile username
  return username
  where usernameFile = "username"

-- Create a user

data CreateUserResponse = CreateUserResponse {
  _username :: Username
} deriving (Show, Generic)

instance FromJSON CreateUserResponse where
  parseJSON = parseJSONIgnoreUnderscore

createUserAPI :: IPAddress -> IO [ApiResponse CreateUserResponse]
createUserAPI ip = postAPI (apiURL ip) $ object ["devicetype" .= deviceName]

createUser :: IPAddress -> IO Username
createUser ip = do
  response <- createUserAPI ip
  case response of
    [ApiSuccess (CreateUserResponse username)] -> return username
    [ApiError 101 _ desc] -> do
      putStrLn desc
      threadDelay (10^(6 :: Int))
      createUser ip
    _ -> error "Unknown error"

-- Delete a user

deleteUserAPI :: IPAddress -> Username -> Username -> IO [ApiResponse String]
deleteUserAPI ip username usernameToDelete
  = deleteAPI (authApiURL ip username ++ "/config/whitelist/" ++ usernameToDelete)

-- Delete all other users created by this app

cleanUsers :: IPAddress -> Username -> IO ()
cleanUsers ip username = do
  Whitelist entries <- getWhitelist ip username
  forM_ (HM.toList entries) $ \(username', WhitelistEntry _ _ deviceName') ->
    when (deviceName' == deviceName && username' /= username) $ do
      putStrLn $ "Deleting user " ++ username'
      void $ deleteUserAPI ip username username'
