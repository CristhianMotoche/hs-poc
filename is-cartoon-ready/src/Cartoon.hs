module Cartoon (Cartoon(..)) where

data Cartoon =
  Cartoon
    { name :: String
    , latestEpisode :: String
    , episodeURL :: String
    } deriving (Show, Eq)
