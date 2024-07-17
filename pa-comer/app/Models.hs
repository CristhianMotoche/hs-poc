-- > :set -XDeriveGeneric -XGADTs -XOverloadedStrings -XFlexibleContexts -XFlexibleInstances -XTypeFamilies -XTypeApplications -XDeriveAnyClass
{-# LANGUAGE DeriveGeneric #-}

module Models where

import Data.Text (Text)
import Database.Beam
import Database.Beam.Sqlite

data MealT f = Meal
  { _mealName :: Columnar f Text,
    _mealDescription :: Columnar f Text,
    _mealType :: Columnar f Text
  }
  deriving (Generic)

type Meal = MealT Identity

type MealId = PrimaryKey MealT Identity
