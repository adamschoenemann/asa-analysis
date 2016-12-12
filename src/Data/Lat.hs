{-
Lattices
-}
module Data.Lat where

import Data.Cmm.AST

-- should also have a top, but we don't use it in any of the analyses - for now!
class (Ord a, Eq a, Show a) => Lat a where
  bottom :: Program -> a
  top    :: Program -> a
  leastUpperBound :: a -> a -> a

data UnitLat = UnitLat deriving (Ord, Eq, Show)

instance Lat UnitLat where
  bottom = const UnitLat
  leastUpperBound _ _ = UnitLat
  top = const UnitLat