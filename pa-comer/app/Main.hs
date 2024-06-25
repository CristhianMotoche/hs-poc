{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.ByteString.Lazy as Lazy
import Data.Proxy
import Network.HTTP.Media ((//), (/:))
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server

-- ^ Servant stuff

data HTML = HTML

newtype RawHtml = RawHtml {unRaw :: Lazy.ByteString}

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender _ = unRaw

type MealsAPI = "meals" :> Get '[HTML] RawHtml
-- ^ My types

newtype Ingredient = Ingredient
  { iName :: String
  }

data Proportion = Proportion
  { pQuantity :: Int,
    pIngredient :: Ingredient
  }

data Recipe = Recipe
  { rName :: String,
    rProportions :: [Proportion]
  }

recipies :: [Recipe]
recipies =
  [ Recipe
      { rName = "Tigrillo",
        rProportions =
          [ Proportion
              { pQuantity = 1,
                pIngredient =
                  Ingredient
                    { iName = "Plantain"
                    }
              }
          ]
      }
  ]

mealHandler :: Handler RawHtml
mealHandler = do
  return (RawHtml $ "<body><i>Hello</i> <strong>World</strong></body>")

myServer :: Server MealsAPI
myServer = mealHandler

port :: Int
port = 8080

main :: IO ()
main = do
  putStrLn $ "Server listening on port " <> show port <> "..."
  run port (serve (Proxy :: Proxy MealsAPI) myServer)
