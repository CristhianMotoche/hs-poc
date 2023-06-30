{-# LANGUAGE RecordWildCards #-}

module GradientDescent where

import qualified Debug.Trace as DT

type LearningRate = Double

data Point =
  Point
    { x :: Double
    , y :: Double
    } deriving (Eq)

instance Show Point where
  show Point{..} = "(" ++ show x ++ "," ++ show y ++ ")"

data Parameters =
  Parameters
    { m :: Double
    , b :: Double
    } deriving (Eq)

instance Show Parameters where
  show Parameters{..} =  "y = " ++ show m ++" x + " ++ show b

sqrError :: Parameters -> Point -> Double
sqrError Parameters{..} Point{..} =
  (y - (m*x + b)) ** 2

totalSqrError :: Parameters -> [Point] -> Double
totalSqrError _ [] = 0.0
totalSqrError parameters points =
  sum (map (sqrError parameters) points) / fromIntegral (length points)

stepGradient :: Parameters -> [Point] -> LearningRate -> Parameters
stepGradient parameters@Parameters{..} points learningRate =
  Parameters
    {
      m = m - learningRate * deriveM parameters points
    , b = b - learningRate * deriveB parameters points
    }
    where
      n = fromIntegral (length points)
      deriveM parameters points = -(2/n) * sum (map (deriveM' parameters) points) 
      deriveM' Parameters{..} Point{..} = x*(y - (m*x + b))
      deriveB parameters points = -(2/n) * sum (map (deriveB' parameters) points) 
      deriveB' Parameters{..} Point{..} = y - (m*x + b)

gradientDescent :: Parameters -> [Point] -> LearningRate -> Int -> Parameters
gradientDescent parameters _ _ 0 = parameters
gradientDescent parameters points learningRate iterations =
  gradientDescent
    (stepGradient parameters points learningRate) points learningRate (iterations - 1) 
