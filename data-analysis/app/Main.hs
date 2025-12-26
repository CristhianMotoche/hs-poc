module Main (main) where

import           DataFrame as D

main :: IO ()
main = do
  df <- D.readCsv "data/Titanic-Dataset.csv"
  print df
