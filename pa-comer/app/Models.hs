{-# LANGUAGE DeriveAnyClass #-}
-- > :set -XDeriveGeneric -XGADTs -XOverloadedStrings -XFlexibleContexts -XFlexibleInstances -XTypeFamilies -XTypeApplications -XDeriveAnyClass
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Models where

import Data.Text (Text)
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple

-- ^ Tables

data MealT f = Meal
  { _mealName :: Columnar f Text,
    _mealDescription :: Columnar f Text,
    _mealType :: Columnar f Text
  }
  deriving (Generic, Beamable)

type Meal = MealT Identity

type MealId = PrimaryKey MealT Identity

instance Table MealT where
  data PrimaryKey MealT f = MealId (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = MealId . _mealName

deriving instance Show Meal

deriving instance Eq Meal
-- ^ Database

data PaComerDb f = PaComerDb
  { _pacomerMeals :: f (TableEntity MealT)
  }
  deriving (Generic, Database be)

paComerDb :: DatabaseSettings be PaComerDb
paComerDb = defaultDbSettings
-- ^ Seed database

meals :: [Meal]
meals =
  [ Meal "Tigrillo" "Platano, queso y huevos" "Desayuno",
    Meal "Seco de pollo" "Pollo, arroz, y encurtido" "Almuerzo",
    Meal "Ensalada de atún" "Atún y vegetales" "Cena"
  ]

seed :: Connection -> IO ()
seed conn =
  runBeamSqliteDebug putStrLn {- for debug output -} conn $
    runInsert $
      insert (_pacomerMeals paComerDb) $
        insertValues meals

allMealsQ = all_ (_pacomerMeals paComerDb)

allMeals conn =
  runBeamSqliteDebug putStrLn conn $ do
    ms <- runSelectReturningList $ select allMealsQ
    mapM_ (liftIO . putStrLn . show) ms
