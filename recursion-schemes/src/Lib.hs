module Lib where

-- | Common way to do recursion, defined explicitly
data Expr = Const Int
          | Add Expr Expr
          | Mul Expr Expr

print :: Expr -> String
print (Const i) = show i
print (Add a b) = print a ++ " + " ++ print b
print (Mul a b) = print a ++ " * " ++ print b

-- | Drawback
--
-- Possible errors
-- GHC is way better in optimizing non-recursive then recursive code
