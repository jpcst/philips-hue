{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Control.Concurrent
import           Control.Monad.Reader
import           System.Random

import           Bridge
import           Group
import           LightState
import           Types
import           User

getSetup :: IO (IPAddress, Username)
getSetup = do
  ip <- getPrimaryBridgeIP
  username <- getUsername ip
  return (ip, username)

newtype Api a = Api {unAPI :: ReaderT (IPAddress, Username) IO a}

instance Functor Api where
  fmap f (Api a) = Api (fmap f a)

instance Applicative Api where
  pure a = Api (pure a)
  Api f <*> Api a = Api (f <*> a)

instance Monad Api where
  a >>= f = do
    a'' <- liftIO $ do
      (ip, username) <- getSetup
      runReaderT (unAPI a) (ip, username)
    f a''

instance MonadIO Api where
  liftIO = Api . liftIO

random :: IPAddress -> Username ->  Int -> IO ()
random ip username transitionTimeInSec = forever $ do
  randomLightStateParam <- unLightState <$> randomIO
  _ <- setGroupStateAPI ip username allLights $
    LightStateParam (setTransitionTime transitionTime : randomLightStateParam)
  threadDelay delay

  where transitionTime = transitionTimeInSec * 10
        delay = transitionTimeInSec * 10^(6 :: Int)
