module Main (main) where

import           DataFrame as D

main :: IO ()
main = do
  df <- D.readCsv "data/Titanic-Dataset.csv"
  print $ D.columnNames df
  print $ D.describeColumns df
  print $ D.take 5 df
