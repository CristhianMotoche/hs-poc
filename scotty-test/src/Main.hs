{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Data.Person
import Data.Text.Lazy (pack)
import System.Environment
import Control.Monad

main :: IO ()
main = do
  port <- liftM read $ getEnv  "PORT"
  scotty port $ do

    let c = Person "21345697" "Camm" 21
    get "/" $ do
      json c

    delete "/" $ do
      html "deleted!"

    post "/" $ do
      myPerson <- jsonData
      text $ pack $ name myPerson

    put "/" $ do
      html "put-ted!"
