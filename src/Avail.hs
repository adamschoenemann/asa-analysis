{-# LANGUAGE NamedFieldPuns, FlexibleInstances #-}

module Avail where

import Control.Monad.Fix
import Data.Set (Set, union, (\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State
import Data.Functor ((<$>))
import Data.List (sort)
import Data.Graph
import Data.Cmm.AST
import Anal.CFG

type Lattice = Set Expr
type TFun = Lattice -> Lattice

-- First go at generalizing lattice elements
class (Ord a, Eq a) => Lat a where
  leastUpperBound :: Lat a => a -> a -> a

instance Lat (Set Expr) where
  leastUpperBound = S.intersection

exprs :: Expr -> Lattice
exprs expr = case expr of
  e@(Add e1 e2) -> S.singleton e `union` exprs e1 `union` exprs e2
  e@(Sub e1 e2) -> S.singleton e `union` exprs e1 `union` exprs e2
  e@(Mul e1 e2) -> S.singleton e `union` exprs e1 `union` exprs e2
  e@(Gt  e1 e2) -> S.singleton e `union` exprs e1 `union` exprs e2
  e@(Lt  e1 e2) -> S.singleton e `union` exprs e1 `union` exprs e2
  e@(Eq  e1 e2) -> S.singleton e `union` exprs e1 `union` exprs e2
  ILit _    -> S.empty
  BLit _    -> S.empty
  Var   _     -> S.empty
  Input       -> S.empty

avail :: Expr -> Lattice -> Lattice
avail e l    = l `union` exprs e

unavail :: String -> Lattice -> Lattice
unavail v l  = l \\ S.filter (occursIn v) l where
  occursIn v e = case e of
    Add e1 e2 -> occursIn v e1 || occursIn v e2
    Sub e1 e2 -> occursIn v e1 || occursIn v e2
    Mul e1 e2 -> occursIn v e1 || occursIn v e2
    Gt  e1 e2 -> occursIn v e1 || occursIn v e2
    Lt  e1 e2 -> occursIn v e1 || occursIn v e2
    Eq  e1 e2 -> occursIn v e1 || occursIn v e2
    ILit _  -> False
    BLit _  -> False
    Var s     -> s == v
    Input     -> False

assign :: String -> Expr -> Lattice -> Lattice
assign v e  = avail e . unavail v

stmtToTFun :: Stmt -> TFun
stmtToTFun stmt = case stmt of
  Skip -> id
  Ass v e -> assign v e
  ITE e s s' -> avail e
  Block _ -> id
  While e s -> avail e
  Output e  -> avail e


prog1 :: [Stmt]
prog1 =
  [ Ass "x" (Var "a" `Mul` Var "b")
  , (ITE ((Var "a" `Mul` Var "b") `Gt` (ILit 20 `Mul` Var "c"))
       (Ass "y" (ILit 20 `Add` Var "a"))
       (Ass "y" (ILit 30 `Add` Var "c"))
  )
  , Ass "z" (ILit 20 `Mul` ILit 30)
  , Ass "a" (ILit 20)
  , Ass "u" (Var "a" `Mul` Var "b")
  ]

prog2 :: [Stmt]
prog2 =
  [ Ass "x" (Var "a" `Mul` Var "b")
  , While ((ILit 20 `Mul` Var "c") `Gt` (Var "a" `Mul` Var "b"))
       (Block [ (Ass "a" (ILit 20 `Add` Var "a"))
              , (Ass "c" (Var "c" `Sub` ILit 1))
              ])
  , Ass "z" (ILit 20 `Mul` ILit 30)
  , Ass "a" (ILit 20)
  , Ass "u" (Var "a" `Mul` Var "b")
  ]


cfg1 :: Graph CFGNodeData CFGEdgeData
cfg1 = Graph {
      nodes = nodes
    , edges = S.fromList $ map (\(u,v,d) -> Edge (u,v) d) edges
    , src = 0, sink = 7
  } where
      nodes = M.fromList $ zipWith (\i a -> (i, Node i a)) [0..]
                [ CFGStmt $ Ass "x" (Var "a" `Mul` Var "b")   -- x := a * b  (0)
                , Cond ((Var "a" `Mul` Var "b") `Gt` (ILit 20 `Mul` Var "c")) -- if (a * b > 20 * c) (1)
                , CFGStmt $ Ass "y" (ILit 20 `Add` Var "a")  -- y := 20 + a (2)
                , CFGStmt $ Ass "y" (ILit 30 `Add` Var "c")  -- y := 30 + c (3)
                , ConfPoint
                , CFGStmt $ Ass "z" (ILit 20 `Mul` ILit 30) -- z := 20 * 30 (4)
                , CFGStmt $ Ass "a" (ILit 20)                -- a := 20 (5)
                , CFGStmt $ Ass "u" (Var "a" `Mul` Var "b")   -- u := a * b (6)
                ]
      no = NoData
      edges = [(0,1,no), (1,2, Branch True), (1,3, Branch False), (3,4,no), (2,4,no), (4,5,no), (5,6,no), (6,7,no)]

cfg1' :: Graph CFGNodeData CFGEdgeData
cfg1' = cfg prog1

cfg2 :: Graph CFGNodeData CFGEdgeData
cfg2 = cfg prog2


data ProgPoint =
  PP { dependent :: Set ID, node :: CFGNodeData }
     deriving (Show)

cfgToProgP :: Graph CFGNodeData CFGEdgeData -> [ProgPoint]
cfgToProgP (Graph {nodes, edges, src, sink}) =
  map nodeToProgP (M.elems nodes) where
    nodeToProgP node@(Node i inner) =
      let dependent = incoming i
      in  PP { dependent = dependent, node = inner }
    incoming i = S.map fst . S.filter ((i == ) . snd) $ S.map endpoints edges


type Equation = [Lattice] -> Lattice

progPsToEqs :: [ProgPoint] -> [Equation]
progPsToEqs points = map pointToEq points where
  singleDepOrEmpty deps
    | S.null deps        = const S.empty
    | S.size deps == 1 = (!! S.elemAt 0 deps)
    | otherwise          = error "Only call this function on singleton  or empty sets :("
  leastUpperBound :: [Lattice] -> Set ID -> Lattice
  leastUpperBound prev deps
    | S.null deps = S.empty
    | otherwise = foldl1 S.intersection . map (\d -> prev !! d) . S.toList $ deps
  pointToEq (PP { dependent, node }) prev =
    case node of
      -- Conditionals and Stmts only have one incoming edge!
      Cond expr     -> avail expr $ singleDepOrEmpty dependent prev
      CFGStmt stmt  -> stmtToTFun stmt $ singleDepOrEmpty dependent prev
      -- Confluence points have more (actually only 2)
      ConfPoint     -> leastUpperBound prev dependent

type BigT = [Lattice] -> [Lattice]

eqsToBigT :: [Equation] -> BigT
eqsToBigT eqs l = map ($ l) eqs

solveFix :: [Lattice] -> BigT -> [Lattice]
solveFix l bigT =
  let l' = bigT l
  in  if (l == l') then l else solveFix l' bigT

collectExprs :: [Stmt] -> Set Expr
collectExprs stmts = foldl union S.empty $ map collectExprs' stmts where
  collectExprs' stmt = case stmt of
    Skip -> S.empty
    Ass _ e -> exprs e
    ITE e t f -> exprs e `union` collectExprs' t `union` collectExprs' f
    Block stmts -> collectExprs stmts --collectExprs' s1 `union` collectExprs' s2
    While e s -> exprs e `union` collectExprs' s
    Output e  -> exprs e


analyzeProg :: [Stmt] -> [Lattice]
analyzeProg prog = solveFix initial bigT where
  allExprs = collectExprs prog
  progps = cfgToProgP $ cfg prog
  bigT = eqsToBigT . progPsToEqs $ progps
  initial = replicate (length progps) allExprs
