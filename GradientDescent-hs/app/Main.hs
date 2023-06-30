module Main where

import Lib
import GradientDescent
import ReadCSV

main :: IO ()
main = do
  points <- readCSV "data/data.csv"
  let learningRate = 0.00001
  let iterations = 1000
  let result = gradientDescent Parameters { m = 0, b = 0 } points learningRate iterations
  let error' = totalSqrError result points
  putStrLn $ "The result is " ++ show result ++ " with error: " ++ show error' ++ "%"

