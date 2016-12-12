{-# LANGUAGE ExistentialQuantification #-}

module Anal
  ( module Anal
  , module Data.Lat
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Cmm.AST
import Data.Cmm.Annotated
import Data.CFG
import Text.Pretty
import Utils
import Data.Lat


-- Transfer Function
type TFun a = a -> a

-- a function from dependencies to a single lattice element
type Equation a = Map ID a -> a

forward :: Lat a => a -> Node -> Map ID a -> a
forward srcEnv node envs =
  case node of
      NSource o              -> srcEnv
      NSingle stmt i o       -> getEnv i
      NITE e i bt bf _   -> getEnv i
      NWhile e i bt bf _ -> getEnv i
      NConfl (i1, i2) o -> leastUpperBound (getEnv i1) (getEnv i2)
      NSink i                -> getEnv i
  where getEnv k = unsafeLookup k envs

backwards :: Lat a => a -> Node -> Map ID a -> a
backwards sinkEnv node envs =
  case node of
      NSource o              -> getEnv o
      NSingle stmt i o       -> getEnv o
      NITE e i bt bf _   -> leastUpperBound (getEnv bt) (getEnv bf)
      NWhile e i bt bf _ -> leastUpperBound (getEnv bt) (getEnv bf)
      NConfl (i1, i2) o -> getEnv o
      NSink i                -> sinkEnv
  where getEnv k = unsafeLookup k envs

data Analysis a
  = Analysis { singleToTFun :: Stmt -> TFun a
             , condToTFun :: Expr -> TFun a
             , initialEnv :: CFG -> a
             , getDeps    :: Node -> Map ID a -> a
             }

idAnalysis :: Lat a => Analysis a
idAnalysis =
  Analysis { singleToTFun = const id
           , condToTFun = const id
           , initialEnv = const bottom
           , getDeps    = forward bottom
           }

cfgToEqs :: Lat a => Analysis a -> CFG -> Map ID (Equation a)
cfgToEqs anal (CFG nodes) = M.mapWithKey nodeToEq nodes where
  nodeToEq k node prev =
    let dep = (getDeps anal) node prev
    in case node of
      NSource o              -> dep
      NSingle stmt i o       -> (singleToTFun anal $ stmt) dep
      NITE e i bt bf _   -> (condToTFun anal $ e)    dep
      NWhile e i bt bf _ -> (condToTFun anal $ e)    dep
      NConfl (i1, i2) o -> dep
      NSink i                -> dep

type BigT a = Map ID a -> Map ID a

eqsToBigT :: Lat a => Map ID (Equation a) -> BigT a
eqsToBigT eqs l = M.map ($ l) eqs

-- solve the recursive equations with the fixpoint theorem!
solveFix :: Lat a => Map ID a -> BigT a -> Map ID a
solveFix l bigT =
  let l' = bigT l
  in  if (l == l') then l else solveFix l' bigT

-- fixpoint operator!
fix :: (a -> a) -> a
fix f =
  let x = f x
  in  x

-- solveFix without explicit recursion
solveFix' :: Lat a => Map ID a -> BigT a -> Map ID a
solveFix' = fix (\f l bigT ->
                    let l' = bigT l
                    in if (l == l') then l else f l' bigT
                )

analyzeCFG :: Lat a => Analysis a -> CFG -> Map ID a
analyzeCFG anal cfg@(CFG nodes) = solveFix initial bigT where
  initialL = (initialEnv anal) cfg
  eqs = cfgToEqs anal cfg
  bigT = eqsToBigT eqs
  initial = M.map (const initialL) nodes

analyzeProg :: Lat a => Analysis a -> Program -> Map ID a
analyzeProg anal prog = analyzeCFG anal (progToCfg prog)

pprintAnalysis :: (Lat a, Pretty a) => Analysis a -> Program -> IO ()
pprintAnalysis anal = mapM_ (\(i, r) -> putStrLn $ (show i ++ ": " ++ ppr r)) .
                        M.toList . analyzeProg anal

printAnalysis :: (Lat a, Show a) => Analysis a -> Program -> IO ()
printAnalysis anal = mapM_ (\(i, r) -> putStrLn $ (show i ++ ": " ++ show r)) .
                        M.toList . analyzeProg anal

-- an analysis and a way to transform the cfg using that analysis
data Optimization = forall a. Lat a =>
  Opt { optTransform  :: [Annotated a] -> Program
      , optAnalysis   :: Analysis  a
      }

runOpt :: Optimization -> Program -> Program
runOpt (Opt trans anal) prog =
  let cfg      = progToCfg prog
      analyzed = analyzeCFG anal cfg
      annotated = cfgToAnnotated analyzed cfg
      optimized = trans annotated
  in optimized

-- run optb first and then opt a
runOpts :: Optimization -> Optimization -> Program -> Program
runOpts opta optb prog =
  let prog'  = runOpt optb prog
  in  runOpt opta prog'

seqOpts :: [Optimization] -> (Program -> Program)
seqOpts [] = id
seqOpts [opt] = runOpt opt
seqOpts (o:os) = foldr (\opt fn -> fn . runOpt opt) (runOpt o) os


optimizeProg :: [Optimization] -> Program -> Program
optimizeProg opts prog =
  let fixpoint =
        fix (\f prog -> let prog' = seqOpts opts prog
                       in  if prog' == prog then prog' else f prog'
        )
  in  fixpoint prog