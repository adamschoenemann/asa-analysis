{-
Main module of the library
Defines and exports the Analysis and Optimization types
Wires the data-flow analysis pipeline together
-}

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


-- Transfer Function (just endofunctions)
type TFun a = a -> a

-- a function from dependencies to a single lattice element
-- or, a function from the previous environment to a single lattice element
-- Used to represent a recursive equation
-- e.g. `x = f x z` is a function that applies f to the previous value of
-- x and z, which we look up in the previous environment
type Equation a = Map ID a -> a

-- The definition of how a forward-analysis proceeds
-- It defines how dependencies between nodes in the CFG propagate
-- (forward in this case)
forward :: Lat a => a -> Node -> Map ID a -> a
forward srcEnv node envs =
  case node of
      NSource o            -> srcEnv
      NSingle stmt i o     -> getEnv i -- single stmts depend on its incoming edge
      NITE e i bt bf end   -> getEnv i -- ite depends on its incoming edge
      NWhile e i bt bf end -> getEnv i -- while depends on its incoming edge
      -- confluences depends on the LUP of its incomign and outgoing
      NConfl (i1, i2) o    -> leastUpperBound (getEnv i1) (getEnv i2)
      -- sink depends on its incoming edge
      NSink i              -> getEnv i
  where getEnv k = unsafeLookup k envs

-- The similar backwards-analysis dependency propagation
backwards :: Lat a => a -> Node -> Map ID a -> a
backwards sinkEnv node envs =
  case node of
      NSource o          -> getEnv o -- source depends on its outgoing edge
      -- single stmts depend on its outgoing edge
      NSingle stmt i o   -> getEnv o
      -- ITE is the LUP of its true and false branch
      NITE e i bt bf _   -> leastUpperBound (getEnv bt) (getEnv bf)
      -- while is the LUP of its true and false branch
      NWhile e i bt bf _ -> leastUpperBound (getEnv bt) (getEnv bf)
      NConfl (i1, i2) o  -> getEnv o -- confluence depends on its outgoing edge
      NSink i            -> sinkEnv -- sink is sinkEnv
  where getEnv k = unsafeLookup k envs

-- The data-type representing an arbitrary analysis
-- It requries two functions:
--    `nodeToTFun` which converts a Node to a transfer-function
--    `getDeps` which determines how dependencies between nodes are resolved
--       This is almost always `backwards sinkEnv` or `forwards srcEnv`
data Analysis a
  = Analysis { nodeToTFun :: Node -> TFun a
             , getDeps    :: Node -> Map ID a -> a
             }

-- The simplest analysis that just constantly forwards the `srcEnv`
idAnalysis :: Lat a => a -> Analysis a
idAnalysis srcEnv =
  Analysis { nodeToTFun = const id
           , getDeps    = forward srcEnv
           }

-- a mapping from a CFG to a `Map` from graph ID to recursive equation
-- This resulting `Map` represents the recursive equations derived from the graph
-- The left-hand-side of the equations is not [a,b...] but the ID of the
-- corresponding node in the graph
-- M.mapWithKey :: (ID -> Node -> Equation a) -> Map ID Node -> Map ID (Equation a)
cfgToEqs :: Lat a => Analysis a -> CFG -> Map ID (Equation a)
cfgToEqs anal (CFG nodes) = M.mapWithKey nodeToEq nodes where
  nodeToEq k node = \prev ->
    let dep = (getDeps anal) node prev
    in  (nodeToTFun anal node) dep

-- The "Big Transfer Function" T((a,b,c,...)) -> (a',b',c',...)
type BigT a = Map ID a -> Map ID a

-- A mapping from the recursive equations to the big transfer function
eqsToBigT :: Lat a => Map ID (Equation a) -> BigT a
eqsToBigT eqs = \env -> M.map (\tfun -> tfun env) eqs

{-
Concrete example of how equations are represented (kind-of) in
our program:

-- in math
equations:
  id1 = ⊥
  id2 = f_{x=0} (id1)
  id3 = id2 ∪ id4
  id4 = f_{x=x+1} (id3)
  id5 = id4

-- in haskell
equations = M.fromList
  [ (id1, \env -> ⊥)
  , (id2, \env -> f_{x=0} (M.lookup id1 env))
  , (id3, \env -> leastUpperBound (M.lookup id2 env) (M.lookup id4 env))
  , (id4, \env -> f_{x=x+1} (M.lookup id3 env))
  , (id5, \env -> M.lookup id4 env)
  ]

(bigT equations) = \env ->
  M.map (\tfun -> tfun env) equations

-}

-- solve the recursive equations with the fixpoint theorem!
-- here, we write it explicitly
solveFix :: Lat a => Map ID a -> BigT a -> Map ID a
solveFix env bigT =
  let env' = bigT env
  in  if (env == env') then env else solveFix env' bigT

-- fixpoint operator!
fix :: (a -> a) -> a
fix f =
  let x = f x -- f (f (f (f (f x)))) and so forth
  in  x


-- solveFix without explicit recursion
-- f :: (Map ID a -> (BigT a -> MapID a) -> (Map ID a -> BigT a -> MapID a))
solveFix' :: Lat a => Map ID a -> BigT a -> Map ID a
solveFix' = fix (\f env bigT ->
                    let env' = bigT env
                    in if (env == env') then env else f env' bigT
                )


-- The star of the show!
-- Given an analysis, it takes a program and returns the result
-- of running this analysis on the program
analyzeProg :: Lat a => Analysis a -> Program -> Map ID a
analyzeProg anal prog = solveFix intialEnv bigT where -- step 5
  cfg@(CFG nodes) = progToCfg prog -- step 1. step 2 is already done by the Analysis
  bot = bottom prog       -- the program-specific bottom
  eqs = cfgToEqs anal cfg -- step 3
  bigT = eqsToBigT eqs -- step 4
  intialEnv = M.map (const bot) nodes -- initial env to use for solving eqs


-- This will just pretty-print an analysis
pprintAnalysis :: (Lat a, Pretty a) => Analysis a -> Program -> IO ()
pprintAnalysis anal = mapM_ (\(i, r) -> putStrLn $ (show i ++ ": " ++ ppr r)) .
                        M.toList . analyzeProg anal

-- This will just print an analysis (not pretty)
printAnalysis :: (Lat a, Show a) => Analysis a -> Program -> IO ()
printAnalysis anal = mapM_ (\(i, r) -> putStrLn $ (show i ++ ": " ++ show r)) .
                        M.toList . analyzeProg anal

-- An Optimization is
-- an analysis and
-- a way to transform the AST using that analysis
-- (order is flipped in this record though)
-- notice the existential type (cool!) introduced with forall
data Optimization = forall a. Lat a =>
  Opt { optTransform  :: [Annotated a] -> Program -- the optimization transformation
      , optAnalysis   :: Analysis  a              -- the analysis
      }

-- given an optimization and a program it returns an optimized program
runOpt :: Optimization -> Program -> Program
runOpt (Opt trans anal) prog =
  let cfg      = progToCfg prog  -- create CFG
      analResult = analyzeProg anal prog -- analyze the program
      -- re-create the original program, but annotated with the result of
      -- the analysis
      annotated = cfgToAnnotated analResult cfg
      -- transform the annotated source, resulting in the optimized program
      optimized = trans annotated
  in optimized

-- runs two optimizations in sequence
-- NOTE: runs optb first and then opt a
-- like composition, i.e. f . g is first g then f
runOpts :: Optimization -> Optimization -> Program -> Program
runOpts opta optb prog =
  let prog'  = runOpt optb prog
  in  runOpt opta prog'

-- run a list of optimizations in sequence (last to first)
seqOpts :: [Optimization] -> (Program -> Program)
seqOpts [] = id
seqOpts [opt] = runOpt opt
seqOpts (o:os) = foldr (\opt fn -> fn . runOpt opt) (runOpt o) os

-- optimize a program with a list of optimizations until
-- no further optimizations are possible.
-- So, another application of the fixed-point theorem, except we have
-- no idea that it will terminate (but we hope so).
optimizeProg :: [Optimization] -> Program -> Program
optimizeProg opts prog =
  let fixpoint =
        fix (\f prog -> let prog' = seqOpts opts prog
                        in  if prog' == prog then prog' else f prog'
        )
  in  fixpoint prog