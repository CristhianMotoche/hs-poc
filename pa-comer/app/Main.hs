module Main where

newtype Ingredient = Ingredient
  { iName :: String
  }

data Proportion = Proportion
  { pQuantity :: Int,
    pIngredient :: Ingredient
  }

data Recipe = Recipe
  { rName :: String,
    rProportions :: [Proportion]
  }

recipies :: [Recipe]
recipies =
  [ Recipe
      { rName = "Tigrillo",
        rProportions =
          [ Proportion
              { pQuantity = 1,
                pIngredient =
                  Ingredient
                    { iName = "Plantain"
                    }
              }
          ]
      }
  ]

main :: IO ()
main = putStrLn "Hello, Haskell!"
