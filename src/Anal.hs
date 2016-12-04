{-# LANGUAGE NamedFieldPuns, FlexibleInstances, GADTs, StandaloneDeriving #-}

module Anal where

import Data.Set (Set, union, (\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Cmm.AST
import Data.CFG
import Data.ProgPoint
import Text.Pretty

class (Ord a, Eq a) => Lat a where
  bottom :: a
  leastUpperBound :: a -> a -> a

-- Transfer Function
type TFun a = a -> a

-- a function from dependencies to a single lattice element
type Equation a = [a] -> a


data Analysis a where
  Analysis :: Lat a => { stmtToTFun :: Stmt -> TFun a
                       , condToTFun :: Expr -> TFun a
                       , initialEnv :: [Stmt] -> a
                       , firstPPEnv :: a -- first program point environment
                       } -> Analysis a

progPsToEqs :: Lat a => Analysis a -> [ProgPoint] -> [Equation a]
progPsToEqs anal points = map pointToEq points where
  pointToEq (PP dependent (k, node)) prev =
    case node of
      Source o              -> firstPPEnv anal
      Single stmt i o       -> (stmtToTFun anal $ stmt) (prev !! i)
      CondITE e i bt bf     -> (condToTFun anal $ e)    (prev !! i)
      CondWhile e i bt bf   -> (condToTFun anal $ e)    (prev !! i)
      Confluence (i1, i2) o -> leastUpperBound (prev !! i1) (prev !! i2)
      Sink i                -> prev !! i

type BigT a = [a] -> [a]

eqsToBigT :: Lat a => [Equation a] -> BigT a
eqsToBigT eqs l = map ($ l) eqs

-- solve the recursive equations with the fixpoint theorem!
solveFix :: Lat a => [a] -> BigT a -> [a]
solveFix l bigT =
  let l' = bigT l
  in  if (l == l') then l else solveFix l' bigT

-- fixpoint operator!
fix :: (a -> a) -> a
fix f =
  let x = f x
  in  x

-- solveFix without explicit recursion
solveFix' :: Lat a => [a] -> BigT a -> [a]
solveFix' = fix (\f l bigT ->
                    let l' = bigT l
                    in if (l == l') then l else f l' bigT
                )


analyzeProg :: Lat a => Analysis a -> [Stmt] -> [(ID, a)]
analyzeProg anal prog = zip (map (fst . node) progps) $ solveFix initial bigT where
  initialL = (initialEnv anal) prog
  progps = cfgToProgP $ progToCfg prog
  bigT = eqsToBigT . progPsToEqs anal $ progps
  initial = replicate (length progps) initialL

pprintAnalysis :: (Lat a, Pretty a) => Analysis a -> [Stmt] -> IO ()
pprintAnalysis anal = mapM_ (\(i, r) -> putStrLn $ (show i ++ ": " ++ ppr r)) .
                        analyzeProg anal

printAnalysis :: (Lat a, Show a) => Analysis a -> [Stmt] -> IO ()
printAnalysis anal = mapM_ (\(i, r) -> putStrLn $ (show i ++ ": " ++ show r)) .
                        analyzeProg anal

