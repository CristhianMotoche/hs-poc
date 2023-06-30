module ReadCSV where

import GradientDescent

readCSV :: FilePath -> IO [Point]
readCSV file = do
  text <- readFile file
  return (map parse (lines text))

parse :: String -> Point
parse string = Point left right
  where
    (dirtyLeft, dirtyRight) = span (/= ',') string
    right = read $ init (tail dirtyRight)
    left = read dirtyLeft
