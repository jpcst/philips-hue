{-# LANGUAGE OverloadedStrings #-}
module Api where

import           Control.Lens
import           Data.Aeson
import           Data.ByteString.Lazy hiding (map)
import           Data.Foldable
import           Network.Wreq         hiding (params)

import           Types

data ApiResponse a
  = ApiSuccess a
  | ApiError {
    errorType   :: Int,
    address     :: String,
    description :: String
  } deriving Show

{-
  An ApiResponse can be a success or an error:
  {
    "success": <value>
  }
  {
    "error": {
      "type": <ID>,
      "address": </resource/parameteraddress>,
      "description": <description>
    }
  }
-}
instance FromJSON a => FromJSON (ApiResponse a) where
  parseJSON = withObject "ApiResponse" $ \v -> asum [
    ApiSuccess <$> v .: "success",
    do errorBody <- v .: "error"
       ApiError
         <$> errorBody .: "type"
         <*> errorBody .: "address"
         <*> errorBody .: "description"
    ]

-- Constructing URLs

apiURL :: IPAddress -> URL
apiURL ip = "http://" ++ ip ++ "/api"

authApiURL :: IPAddress -> Username -> URL
authApiURL ip username = apiURL ip ++ "/" ++ username

-- General API requests
-- TODO: Use persistent connections

getResponse :: FromJSON a => IO (Response ByteString) -> IO a
getResponse response = do
  response' <- asJSON =<< response
  return $ response' ^. responseBody

getAPI :: FromJSON a => URL -> IO a
getAPI url = getResponse $ get url

postAPI :: FromJSON a => URL -> Value -> IO a
postAPI url params = getResponse $ post url params

putAPI :: FromJSON a => URL -> Value -> IO a
putAPI url params = getResponse $ put url params

deleteAPI :: FromJSON a => URL -> IO a
deleteAPI url = getResponse $ delete url
