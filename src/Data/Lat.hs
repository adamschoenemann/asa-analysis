{-
This module defines lattice type class (Lat)
-}
module Data.Lat where

import Data.Cmm.AST

-- A lattice has a bottom, top, and least-upper-bound operator
-- Notice that a must be a partial order (Ord a, Eq a)
class (Ord a, Eq a, Show a) => Lat a where
  bottom :: Program -> a
  top    :: Program -> a
  leastUpperBound :: a -> a -> a

-- The simplest lattice with just one element
data UnitLat = UnitLat deriving (Ord, Eq, Show)

-- Everything is just unit
instance Lat UnitLat where
  bottom = const UnitLat
  leastUpperBound _ _ = UnitLat
  top = const UnitLat

-- Equivalent to UnitLat
instance Lat () where
  bottom = const ()
  leastUpperBound _ _ =  ()
  top = const ()