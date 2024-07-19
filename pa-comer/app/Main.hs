{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.ByteString.Lazy as Lazy
import Data.Proxy
import Database.SQLite.Simple
import Models
import Network.HTTP.Media ((//), (/:))
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import System.Environment

-- ^ Servant stuff

data HTML = HTML

newtype RawHtml = RawHtml {unRaw :: Lazy.ByteString}

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

type MealsAPI = "meals" :> Get '[HTML] RawHtml
-- ^ Servant Handlers

mealHandler :: Handler RawHtml
mealHandler = do
  return (RawHtml $ "<body><i>Hello</i> <strong>World</strong></body>")

myServer :: Server MealsAPI
myServer = mealHandler

port :: Int
port = 8080

dbFile :: String
dbFile = "pacomer.db"
-- ^  Main

main :: IO ()
main = do
  putStrLn $ "Openning DB File" <> dbFile
  args <- getArgs
  conn <- open dbFile
  if "seed" `Prelude.elem` args
    then seed conn
    else do
      allMeals conn
      putStrLn $ "Server listening on port " <> show port <> "..."
      run port (serve (Proxy :: Proxy MealsAPI) myServer)
