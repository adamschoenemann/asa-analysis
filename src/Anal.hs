{-# LANGUAGE NamedFieldPuns, FlexibleInstances, GADTs, StandaloneDeriving #-}

module Anal where

import Data.Set (Set, union, (\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Graph
import Data.Cmm.AST
import Anal.CFG
import Text.Pretty

class (Ord a, Eq a) => Lat a where
  bottom :: a
  leastUpperBound :: [a] -> a

-- Transfer Function
type TFun a = a -> a

data ProgPoint =
  PP { dependent :: Set ID, node :: Node CFGNodeData }
     deriving (Show)

cfgToProgP :: Graph CFGNodeData CFGEdgeData -> [ProgPoint]
cfgToProgP (Graph {nodes, edges, src, sink}) =
  map nodeToProgP (M.elems nodes) where
    nodeToProgP node@(Node i inner) =
      let dependent = incoming i
      in  PP { dependent = dependent, node = node }
    incoming i = S.map fst . S.filter ((i == ) . snd) $ S.map endpoints edges

-- a function from dependencies to a single lattice element
type Equation a = [a] -> a


data Analysis a where
  Analysis :: Lat a => { stmtToTFun :: Stmt -> TFun a
                       , exprToTFun :: Expr -> TFun a
                       , initialEnv :: [Stmt] -> a
                       , firstPPEnv :: a -- first program point environment
                       } -> Analysis a

progPsToEqs :: Lat a => Analysis a -> [ProgPoint] -> [Equation a]
progPsToEqs anal points = map pointToEq points where
  singleDepOrEmpty deps
    | S.null deps        = const (firstPPEnv anal)
    | S.size deps == 1 = let h = S.elemAt 0 deps in (!! h)
    | otherwise          = error "Only call this function on singleton or empty sets :("
  findDeps :: [a] -> Set ID -> [a]
  findDeps prev deps
    | S.null deps = []
    | otherwise = map (\d -> prev !! d) . S.toList $ deps
  pointToEq (PP { dependent, node }) prev =
    let Node i nd = node
    in  case nd of
          -- Conditionals and Stmts only have one incoming edge!
          Cond expr     -> (exprToTFun anal) expr $ singleDepOrEmpty dependent prev
          CFGStmt stmt  -> (stmtToTFun anal) stmt $ singleDepOrEmpty dependent prev
          -- Confluence points have more (actually only 2)
          ConfPoint     -> leastUpperBound (findDeps prev dependent)

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
analyzeProg anal prog = zip (map (getID . node) progps) $ solveFix initial bigT where
  initialL = (initialEnv anal) prog
  progps = cfgToProgP $ cfg prog
  bigT = eqsToBigT . progPsToEqs anal $ progps
  initial = replicate (length progps) initialL

pprintAnalysis :: (Lat a, Pretty a) => Analysis a -> [Stmt] -> IO ()
pprintAnalysis anal = mapM_ (\(i, r) -> putStrLn $ (show i ++ ": " ++ ppr r)) .
                        analyzeProg anal

printAnalysis :: (Lat a, Show a) => Analysis a -> [Stmt] -> IO ()
printAnalysis anal = mapM_ (\(i, r) -> putStrLn $ (show i ++ ": " ++ show r)) .
                        analyzeProg anal

