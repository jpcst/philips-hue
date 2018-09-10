module Main where

import           Lib

main :: IO ()
main = do
  (ip, username) <- getSetup
  print (ip, username)
