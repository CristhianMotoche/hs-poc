{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Models where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Int
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Time.Calendar.OrdinalDate
import Data.Time.LocalTime
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import System.Random

-- ^ Tables

data MenuT f = Menu
  { _menuId :: Columnar f Int32,
    _menuTime :: Columnar f LocalTime
  }
  deriving (Generic, Beamable)

type Menu = MenuT Identity

type MenuId = PrimaryKey MenuT Identity

instance Table MenuT where
  data PrimaryKey MenuT f = MenuId (Columnar f Int32) deriving (Generic, Beamable)
  primaryKey = MenuId . _menuId

data MealForMenuT f = MealForMenu
  { _mealformenuId :: Columnar f Int32,
    _mealformenuMenu :: PrimaryKey MenuT f,
    _mealformenuMeal :: PrimaryKey MealT f
  }
  deriving (Generic, Beamable)

type MealForMenuId = PrimaryKey MealForMenuT Identity

instance Table MealForMenuT where
  data PrimaryKey MealForMenuT f = MealForMenuId (Columnar f Int32) deriving (Generic, Beamable)
  primaryKey = MealForMenuId . _mealformenuId

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

instance ToJSON Meal where
  toJSON (Meal name desc type_) =
    object
      [ "name" .= name,
        "description" .= desc,
        "type" .= type_
      ]

deriving instance Show Meal

deriving instance Eq Meal
-- ^ Database

data PaComerDb f = PaComerDb
  { _pacomerMeals :: f (TableEntity MealT),
    _pacomerMenus :: f (TableEntity MenuT),
    _pacomerMealForMenus :: f (TableEntity MealForMenuT)
  }
  deriving (Generic, Database be)

paComerDb :: DatabaseSettings be PaComerDb
paComerDb = defaultDbSettings
-- ^ Seed database

initialMeals :: [Meal]
initialMeals =
  [ Meal "Tigrillo" "Platano, queso y huevos" "Desayuno",
    Meal "Seco de pollo" "Pollo, arroz, y encurtido" "Almuerzo",
    Meal "Ensalada de atún" "Atún y vegetales" "Cena"
  ]

seed :: Connection -> IO ()
seed conn =
  runBeamSqliteDebug putStrLn {- for debug output -} conn $
    runInsert $
      insert (_pacomerMeals paComerDb) $
        insertValues initialMeals

allMeals :: Connection -> IO [Meal]
allMeals conn =
  runBeamSqliteDebug putStrLn conn $
    runSelectReturningList $
      select allMealsQ
  where
    allMealsQ = all_ (_pacomerMeals paComerDb)

todaysMenu :: Connection -> Day -> IO [(Menu, Meal)]
todaysMenu conn day =
  fmap getFromDay
    <$> runBeamSqliteDebug putStrLn conn
    $ runSelectReturningList
    $ select
    $ manyToMany_
      (_pacomerMealForMenus paComerDb)
      _mealformenuMenu
      _mealformenuMeal
      (all_ (_pacomerMenus paComerDb))
      (all_ (_pacomerMeals paComerDb))
  where
    -- Work around due to: https://stackoverflow.com/q/78858898/5888408
    getFromDay =
      filter (\(menu, _) -> localDay (_menuTime menu) == day)

getFirstMealByType :: Connection -> Text -> IO (Maybe Meal)
getFirstMealByType conn type_ = do
  meals <- getMealsByType
  idx <- getStdRandom (uniformR (0, length meals - 1))
  print idx
  return $ meals !? idx
  where
    getMealsByType =
      runBeamSqliteDebug putStrLn conn $
        runSelectReturningList $
          select $
            do
              meals <- all_ (_pacomerMeals paComerDb)
              guard_ (_mealType meals ==. val_ type_)
              return meals

insertMenu :: Connection -> IO (Either String ())
insertMenu conn = do
  (mbf, mln, mdn) <-
    (,,)
      <$> getFirstMealByType conn "Desayuno"
      <*> getFirstMealByType conn "Almuerzo"
      <*> getFirstMealByType conn "Cena"
  runBeamSqliteDebug putStrLn {- for debug output -} conn $ do
    mmenu <-
      fmap listToMaybe
        <$> runInsertReturningList
        $ insertReturning (_pacomerMenus paComerDb)
        $ insertExpressions [Menu default_ currentTimestamp_]

    case (mmenu, mbf, mln, mdn) of
      (Just menu, Just bf, Just ln, Just dn) ->
        fmap pure $
          runInsert $
            insert (_pacomerMealForMenus paComerDb) $
              insertExpressions
                [ MealForMenu default_ (val_ $ pk menu) (val_ $ pk bf),
                  MealForMenu default_ (val_ $ pk menu) (val_ $ pk ln),
                  MealForMenu default_ (val_ $ pk menu) (val_ $ pk dn)
                ]
      (Nothing, _, _, _) -> fail "There is no menu"
      _ -> fail "There are no meals"

-- utils
-- Taken from Data.List in base-4.19
-- Need to update to GHC 9.8 for that
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0 = Nothing
  | otherwise =
      foldr
        ( \x r k -> case k of
            0 -> Just x
            _ -> r (k - 1)
        )
        (const Nothing)
        xs
        n
