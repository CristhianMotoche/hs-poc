{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import qualified Data.Aeson           as A
import           Data.Aeson.Schema    (Object, get, schema)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.IO         as T

type Data = [schema|
  {
    data: List #Card
  }
|]

type Card = [schema|
  {
    id: Int,
    name: Text,
    type: Text,
    desc: Text,
    atk: Int,
    def: Int,
    level: Int,
    race: Text,
    attribute: Text,
    archetype: Text,
    card_sets: List #CardSet,
    card_images: List #CardImage,
    card_prices: List #CardPrice,
  }
|]

type CardSet = [schema|
  {
    set_name: Text,
    set_code: Text,
    set_rarity: Text,
    set_rarity_code: Text,
    set_price: Text,
  }
|]

type CardImage = [schema|
  {
    id: Int,
    image_url: Text,
    image_url_small: Text,
  }
|]

type CardPrice = [schema|
  {
    cardmarket_price: Text,
    tcgplayer_price: Text,
    ebay_price: Text,
    amazon_price: Text,
    coolstuffinc_price: Maybe Text,
  }
|]


printCardInfo :: Object Card -> IO ()
printCardInfo card = do
  putStrLn $ "id: " ++ show [get| card.id |]
  putStrLn $ "name: " ++ show [get| card.name |]
  putStrLn $ mconcat [
    "atk: ", show [get| card.atk |], " ",
    "def: ", show [get| card.def |]
    ]
  T.putStrLn $ mconcat $
    ["Small image url: "] ++ [get| card.card_images[].image_url_small |]
  putStrLn $
    "Price In Coolstuff.inc: " ++ show [get| card.card_prices[].coolstuffinc_price |]


main :: IO ()
main =
  let decodeStr :: BS.ByteString -> Either String (Object Data)
      decodeStr = A.eitherDecode
   in do
     eData <- decodeStr <$> BS.readFile "time-wizard.json"
     case eData of
       Left str  -> putStrLn $ "Error: " ++ str
       Right dat -> mapM_ printCardInfo [get| dat.data[] |]
