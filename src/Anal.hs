{-# LANGUAGE NamedFieldPuns, FlexibleInstances, GADTs, StandaloneDeriving
           , ExistentialQuantification #-}

module Anal
  ( module Anal
  , module Data.Lat
  ) where

import Data.Set (Set, union, (\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Cmm.AST
import Data.Cmm.Annotated
import Data.CFG
import Data.ProgPoint
import Text.Pretty
import Utils
import Control.Monad.State (runState, get, modify, State)
import Debug.Trace
import Data.Functor ((<$>))
import Data.Lat


-- Transfer Function
type TFun a = a -> a

-- a function from dependencies to a single lattice element
type Equation a = Map ID a -> a


data Analysis a where
  Analysis :: Lat a => { stmtToTFun :: Stmt -> TFun a
                       , condToTFun :: Expr -> TFun a
                       , initialEnv :: CFG -> a
                       , firstPPEnv :: a -- first program point environment
                       } -> Analysis a

idAnalysis :: Lat a => Analysis a
idAnalysis =
  Analysis { stmtToTFun = const id
           , condToTFun = const id
           , initialEnv = const bottom
           , firstPPEnv = bottom
           }

cfgToProgP :: CFG -> Map ID ProgPoint
cfgToProgP (CFG nodes) = M.mapWithKey nodeToPP $ nodes where
  nodeToPP k node = case node of
      Source o              -> PP empty    (k,node)
      Single stmt i o       -> PP (pure i) (k,node)
      CondITE e i bt bf _   -> PP (pure i) (k,node)
      CondWhile e i bt bf _ -> PP (pure i) (k,node)
      Confluence (i1, i2) o -> PP (S.fromList [i1, i2]) (k,node)
      Sink i                -> PP (pure i) (k,node)
    where pure x = S.singleton x
          empty  = S.empty

progPsToEqs :: Lat a => Analysis a -> Map ID ProgPoint -> Map ID (Equation a)
progPsToEqs anal points = M.map pointToEq points where
  pointToEq (PP dependent (k, node)) prev =
    case node of
      Source o              -> firstPPEnv anal
      Single stmt i o       -> (stmtToTFun anal $ stmt) (unsafeLookup i prev)
      CondITE e i bt bf _   -> (condToTFun anal $ e)    (unsafeLookup i prev)
      CondWhile e i bt bf _ -> (condToTFun anal $ e)    (unsafeLookup i prev)
      Confluence (i1, i2) o -> leastUpperBound (unsafeLookup i1 prev) (unsafeLookup i2 prev)
      Sink i                -> unsafeLookup i prev

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
  progps = cfgToProgP $ cfg
  bigT = eqsToBigT . progPsToEqs anal $ progps
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
  , transStmt :: a -> Stmt -> Stmt
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
        Source o              -> Source o
        Single s i o          -> Single    ((transStmt transformer) (getEnv i) s) i o
        CondITE e i bt bf c   -> CondITE   ((transExpr transformer) (getEnv i) e) i bt bf c
        CondWhile e i bt bf c -> CondWhile ((transExpr transformer) (getEnv i) e) i bt bf c
        Confluence (i1, i2) o -> Confluence (i1, i2) o
        Sink i                -> Sink i
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