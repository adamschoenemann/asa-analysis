{-# LANGUAGE NamedFieldPuns, FlexibleInstances #-}

module Anal where

import Control.Monad.Fix
import Data.Set (Set, union, (\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Graph
import Data.Cmm.AST
import Anal.CFG
import Text.Pretty
import Data.List (intercalate)

type Lattice = Set Expr
-- Transfer Function
type TFun = Lattice -> Lattice

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


type Equation = [Lattice] -> Lattice

data Analysis =
  Analysis { stmtToTFun :: Stmt -> TFun
           , exprToTFun :: Expr -> TFun
           , leastUpperBound :: [Lattice] -> Lattice
           , initialLattice :: [Stmt] -> Lattice
           }

progPsToEqs :: Analysis -> [ProgPoint] -> [Equation]
progPsToEqs anal points = map pointToEq points where
  singleDepOrEmpty deps
    | S.null deps        = const S.empty
    | S.size deps == 1 = (!! S.elemAt 0 deps)
    | otherwise          = error "Only call this function on singleton  or empty sets :("
  findDeps :: [Lattice] -> Set ID -> [Lattice]
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
          ConfPoint     -> (leastUpperBound anal) (findDeps prev dependent)

type BigT = [Lattice] -> [Lattice]

eqsToBigT :: [Equation] -> BigT
eqsToBigT eqs l = map ($ l) eqs

solveFix :: [Lattice] -> BigT -> [Lattice]
solveFix l bigT =
  let l' = bigT l
  in  if (l == l') then l else solveFix l' bigT


analyzeProg :: Analysis -> [Stmt] -> [(ID, Lattice)]
analyzeProg anal prog = zip (map (getID . node) progps) $ solveFix initial bigT where
  allExprs = (initialLattice anal) prog
  progps = cfgToProgP $ cfg prog
  bigT = eqsToBigT . progPsToEqs anal $ progps
  initial = replicate (length progps) allExprs

pprintAnalysis :: Analysis -> [Stmt] -> IO ()
pprintAnalysis anal = mapM_ (\(i, r) -> putStrLn $
                        (show i ++ ": " ++
                          (intercalate ",   " . map ppr . S.toList $ r))) .
                        analyzeProg anal

printAnalysis :: Analysis -> [Stmt] -> IO ()
printAnalysis anal = mapM_ (\(i, r) -> putStrLn $
                        (show i ++ ": " ++
                          (intercalate ",   " . map show . S.toList $ r))) .
                        analyzeProg anal