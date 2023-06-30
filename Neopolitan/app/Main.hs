module Main where

import           System.Environment (getArgs)

import           Neo                (Status (..), StatusResponse (..),
                                     readConfig, updateStatus)


lunch :: Status
lunch = Status {
    text = "Lunch"
  , emoji = ":canned_food:"
  , expiration = 3600
  }

main :: IO ()
main = do
  config <- readConfig
  args <- getArgs
  resp' <- updateStatus config lunch
  case resp' of
    Nothing -> putStrLn "Error parsing"
    Just resp ->
      if ok resp
         then putStrLn "Status updated"
         else putStrLn "Nope!"
