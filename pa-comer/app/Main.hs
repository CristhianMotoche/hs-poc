{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as Lazy8
import Data.Proxy
import qualified Data.Text as T
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

fmtMeal :: Meal -> String
fmtMeal meal =
  T.unpack $
    mconcat $
      [ "<p>",
        _mealName meal,
        "</p>",
        "<p>",
        _mealDescription meal,
        "</p>",
        "<p>",
        _mealType meal,
        "</p>"
      ]

mealHandler :: Connection -> Handler RawHtml
mealHandler conn = do
  meals <- liftIO $ allMeals conn
  return $
    RawHtml $
      mconcat $
        [ "<html>",
          "<head>",
          "<meta charset='utf-8'>",
          "</head>",
          "<body>"
        ]
          ++ map (Lazy8.pack . fmtMeal) meals
          ++ [ "</html>",
               "</body>"
             ]

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
