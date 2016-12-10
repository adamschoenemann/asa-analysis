{-# LANGUAGE ExistentialQuantification #-}

module Anal
  ( module Anal
  , module Data.Lat
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Cmm.AST
import Data.Cmm.Annotated
import Data.CFG
import Text.Pretty
import Utils
import Control.Monad.State (runState, modify)
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
  = Analysis { singleToTFun :: SingleStmt -> TFun a
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

analyzeProg :: Lat a => Analysis a -> [Stmt] -> Map ID a
analyzeProg anal prog = analyzeCFG anal (progToCfg prog)

pprintAnalysis :: (Lat a, Pretty a) => Analysis a -> [Stmt] -> IO ()
pprintAnalysis anal = mapM_ (\(i, r) -> putStrLn $ (show i ++ ": " ++ ppr r)) .
                        M.toList . analyzeProg anal

printAnalysis :: (Lat a, Show a) => Analysis a -> [Stmt] -> IO ()
printAnalysis anal = mapM_ (\(i, r) -> putStrLn $ (show i ++ ": " ++ show r)) .
                        M.toList . analyzeProg anal


-- a graph transformation that *preserves* the shape of the graph, but transforms
-- the data in the nodes
data NodeTrans a = NodeTrans
  { transExpr :: a -> Expr -> Expr
  , transStmt :: a -> SingleStmt -> SingleStmt
  }

idNodeTrans :: NodeTrans a
idNodeTrans = NodeTrans { transExpr = const id , transStmt = const id }

idGraphTrans = id

nodeTransCfg :: Lat a => NodeTrans a -> CFG -> Map ID a -> CFG
nodeTransCfg transformer (CFG nodes) envMap =
  let (_, newNodes) = runState (helper S.empty (getNode 0)) nodes
  in  CFG newNodes where
    helper explored (k, node)
      | k `S.member` explored = return ()
      | otherwise = do
          let node' = trans node
          modify (M.insert k node')
          let explored' = S.insert k explored
          mapM_ (helper explored' . getNode) $ getOutgoing node
    trans node =
      case node of
        NSource o              -> NSource o
        NSingle s i o          -> NSingle ((transStmt transformer) (getEnv i) s) i o
        NITE e i bt bf c   -> NITE   ((transExpr transformer) (getEnv i) e) i bt bf c
        NWhile e i bt bf c -> NWhile ((transExpr transformer) (getEnv i) e) i bt bf c
        NConfl (i1, i2) o -> NConfl (i1, i2) o
        NSink i                -> NSink i
    getNode i = (i,unsafeLookup i nodes)
    getEnv i = unsafeLookup i envMap

-- an analysis and a way to transform the cfg using that analysis
data Optimization = forall a. Lat a =>
  Opt { optTransform  :: [Annotated a] -> [Stmt]
      , optAnalysis   :: Analysis  a
      }

runOpt :: Optimization -> [Stmt] -> [Stmt]
runOpt (Opt trans anal) prog =
  let cfg      = progToCfg prog
      analyzed = analyzeCFG anal cfg
      annotated = cfgToAnnotated analyzed cfg
      optimized = trans annotated
  in optimized

-- run optb first and then opt a
runOpts :: Optimization -> Optimization -> [Stmt] -> [Stmt]
runOpts opta optb prog =
  let prog'  = runOpt optb prog
  in  runOpt opta prog'

seqOpts :: [Optimization] -> ([Stmt] -> [Stmt])
seqOpts [] = id
seqOpts [opt] = runOpt opt
seqOpts (o:os) = foldr (\opt fn -> fn . runOpt opt) (runOpt o) os


optimizeProg :: [Optimization] -> [Stmt] -> [Stmt]
optimizeProg opts prog =
  let fixpoint =
        fix (\f prog -> let prog' = seqOpts opts prog
                       in  if prog' == prog then prog' else f prog'
        )
  in  fixpoint prog

-- optimizeProgram :: [Optimization] -> [Stmt] -> [Stmt]
-- optimizeProgram opts prog = cfgToProgram $ optimizeCfg opts (progToCfg prog)