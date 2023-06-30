{-# LANGUAGE DeriveGeneric #-}

module Data.Person where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

data Person = Person { id   :: String
                     , name :: String
                     , age  :: Int
                     } deriving (Show, Generic)

instance ToJSON Person

instance FromJSON Person

