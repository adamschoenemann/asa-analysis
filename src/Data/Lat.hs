
module Data.Lat where

class (Ord a, Eq a, Show a) => Lat a where
  bottom :: a
  leastUpperBound :: a -> a -> a

data UnitLat = UnitLat deriving (Ord, Eq, Show)

instance Lat UnitLat where
  bottom = UnitLat
  leastUpperBound _ _ = UnitLat