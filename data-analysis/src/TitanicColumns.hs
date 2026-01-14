{-# LANGUAGE TemplateHaskell #-}
module TitanicColumns where

import DataFrame as D
import DataFrame.Functions (declareColumnsFromCsvFile)
import Data.Text (Text)

-- Read the CSV and declare columns at compile time
-- This generates column accessor functions
titanicDf :: IO DataFrame
titanicDf = D.readCsv "data/Titanic-Dataset.csv"

-- This would generate functions like:
-- passengerIdCol :: Expr Int
-- survivedCol :: Expr Int  
-- pclassCol :: Expr Int
-- nameCol :: Expr Text
-- sexCol :: Expr Text
-- ageCol :: Expr (Maybe Double)
-- sibspCol :: Expr Int
-- parchCol :: Expr Int
-- ticketCol :: Expr Text
-- fareCol :: Expr (Maybe Double)
-- cabinCol :: Expr (Maybe Text)
-- embarkedCol :: Expr (Maybe Text)

-- Note: You would normally use $(declareColumns df) here,
-- but since df is loaded at runtime, we can't use it directly
-- in Template Haskell. Instead, you'd use declareColumnsFromCsvFile:

-- This generates the column accessors at compile time:
$(declareColumnsFromCsvFile "data/Titanic-Dataset.csv")