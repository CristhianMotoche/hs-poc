{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified DataFrame as D
import qualified DataFrame.Functions as DF

$(DF.declareColumnsFromCsvFile "data/Carnaval Brasil - Gastos.csv")

main :: IO ()
main = do
  df <- D.readCsv "data/Carnaval Brasil - Gastos.csv"

  -- How much did we spend?

  print $ D.sum precio__ df
