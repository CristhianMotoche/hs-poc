module Main (main) where

import           DataFrames as D

main :: IO ()
main = do
  df <- D.readCsv "../data/Titanic-Dataset.csv"
  pure $ D.describeColumns df
