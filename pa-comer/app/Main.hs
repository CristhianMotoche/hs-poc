{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class
import Data.Aeson (ToJSON (toJSON), object, (.=))
import qualified Data.ByteString.Lazy as Lazy
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Database.SQLite.Simple
import Models
import Network.HTTP.Media ((//), (/:))
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import System.Environment
import Text.Mustache

-- ^ Servant stuff

data HTML = HTML

newtype RawHtml = RawHtml {unRaw :: Lazy.ByteString}

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

type MealsAPI = "meals" :> Get '[HTML] RawHtml
-- ^ Servant Handlers

mealHandler :: Connection -> Handler RawHtml
mealHandler conn = do
  template <- liftIO $ compileMustacheDir "main" "templates"
  meals <- liftIO $ allMeals conn
  return $
    RawHtml . TL.encodeUtf8 $
      renderMustache template (toJSON $ object ["meals" .= meals])

myServer :: Connection -> Server MealsAPI
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
  if "seed" `elem` args
    then seed conn
    else do
      putStrLn $ "Server listening on port " <> show port <> "..."
      run port (serve (Proxy :: Proxy MealsAPI) $ myServer conn)
