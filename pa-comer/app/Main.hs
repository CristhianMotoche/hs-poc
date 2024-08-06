{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class
import Data.Aeson (ToJSON (toJSON), object, (.=))
import qualified Data.ByteString.Lazy as Lazy
import Data.Foldable (find)
import Data.Proxy
import Data.String (IsString)
import qualified Data.Text.Lazy.Encoding as TL
import Database.SQLite.Simple
import GHC.TypeLits
import Models
import Network.HTTP.Media ((//), (/:))
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger
import Servant.API
import Servant.Server
import System.Environment
import Text.Mustache
import Web.FormUrlEncoded

-- ^ Some types

type MenuForm = Form
-- ^ Servant stuff

type PostRedirect (code :: Nat) loc =
  Verb 'POST code '[HTML] (Headers '[Header "Location" loc] NoContent)

redirect ::
  (ToHttpApiData loc) =>
  -- | what to put in the 'Location' header
  loc ->
  Handler (Headers '[Header "Location" loc] NoContent)
redirect a = return (addHeader a NoContent)

data HTML = HTML

newtype RawHtml = RawHtml {unRaw :: Lazy.ByteString}

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

type MealsAPI =
  Get '[HTML] RawHtml
    :<|> "meals" :> Get '[HTML] RawHtml
    :<|> "menu" :> ReqBody '[FormUrlEncoded] MenuForm :> PostRedirect 301 String
-- ^ Servant Handlers

mealHandler :: Connection -> Handler RawHtml
mealHandler conn = do
  template <- liftIO $ compileMustacheDir "main" "templates"
  meals <- liftIO $ allMeals conn
  return $
    RawHtml . TL.encodeUtf8 $
      renderMustache template (toJSON $ object ["meals" .= meals])

rootHandler :: Connection -> Handler RawHtml
rootHandler conn = do
  template <- liftIO $ compileMustacheDir "root" "templates"
  meals <- liftIO $ allMeals conn
  let breakfast = find (\x -> _mealType x == "Desayuno") meals
      lunch = find (\x -> _mealType x == "Almuerzo") meals
      dinner = find (\x -> _mealType x == "Cena") meals
  return $
    RawHtml . TL.encodeUtf8 $
      renderMustache template $
        toJSON $
          object
            [ "breakfast" .= breakfast,
              "lunch" .= lunch,
              "dinner" .= dinner
            ]

postMenuHandler :: (ToHttpApiData loc, IsString loc) => Connection -> MenuForm -> Handler (Headers '[Header "Location" loc] NoContent)
postMenuHandler conn _ = do
  liftIO $ insertMenu conn
  redirect "/"

myServer :: Connection -> Server MealsAPI
myServer conn =
  rootHandler conn
    :<|> mealHandler conn
    :<|> postMenuHandler conn

port :: Int
port = 8080

app :: Connection -> Application
app conn = serve (Proxy :: Proxy MealsAPI) $ myServer conn

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
      withStdoutLogger $ \aplogger -> do
        let settings = setPort port $ setLogger aplogger defaultSettings
        runSettings settings (app conn)
