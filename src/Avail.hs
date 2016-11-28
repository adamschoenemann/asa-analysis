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

import Debug.Trace

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Gt  Expr Expr
  | Const Int
  | Var String
  | Input
  deriving (Eq, Ord)

ppExpr expr = case expr of
  Add e1 e2 -> ppExpr e1 ++ " + " ++ ppExpr e2
  Sub e1 e2 -> ppExpr e1 ++ " - " ++ ppExpr e2
  Mul e1 e2 -> ppExpr e1 ++ " * " ++ ppExpr e2
  Gt  e1 e2 -> ppExpr e1 ++ " > " ++ ppExpr e2
  Const x   -> show x
  Var n     -> n
  Input     -> "input"

instance Show Expr where
  show = ppExpr

data Stmt
  = Skip
  | Ass String Expr
  | ITE Expr Stmt Stmt
  | Comp Stmt Stmt
  | While Expr Stmt
  deriving (Eq, Ord)

ppStmt n stmt = case stmt of
  Skip -> "skip"
  Ass v e -> v ++ " := " ++ ppExpr e
  ITE e s1 s2 -> "if " ++ ppExpr e ++ " then " ++ ppStmt n s1 ++ " else " ++ ppStmt n s2
  Comp s1 s2 -> let ident = replicate n '\t' in ppStmt n s1 ++ ";\n" ++ ident ++ ppStmt n s2
  While e s  -> let ident = replicate (n+1) '\t'
                in  "while (" ++ ppExpr e ++ ") do (\n" ++ ident ++ ppStmt (n+1) s ++ ";\n)"

instance Show Stmt where
  show = ppStmt 0

-- Composition to list of statements
comp2list :: Stmt -> [Stmt]
comp2list (Comp s1 c2@(Comp s2 s3)) = s1 : comp2list c2
comp2list (Comp c1@(Comp s1 s2) s3) = comp2list c1 ++ [s3]
comp2list (Comp s1 s2) = [s1,s2]
comp2list _ = error "only flatten compositions"

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
  Const _     -> S.empty
  Var   _     -> S.empty
  Input       -> S.empty

avail e l    = l `union` exprs e
unavail v l  = l \\ S.filter (occursIn v) l where
  occursIn v e = case e of
    Add e1 e2 -> occursIn v e1 || occursIn v e2
    Sub e1 e2 -> occursIn v e1 || occursIn v e2
    Mul e1 e2 -> occursIn v e1 || occursIn v e2
    Gt  e1 e2 -> occursIn v e1 || occursIn v e2
    Const _   -> False
    Var s     -> s == v
    Input     -> False

assign v e  = avail e . unavail v

stmtToTFun :: Stmt -> TFun
stmtToTFun stmt = case stmt of
  Skip -> id
  Ass v e -> assign v e
  ITE e s s' -> avail e
  Comp s s' -> id
  While e s -> avail e

type ID = Int
data Node       a = Node ID a deriving (Show)
type Nodes      a = Map ID (Node a)

-- control flow graph
data CFG a = CFG
  { nodes  :: Nodes a --Map ID (Node a)
  , edges  :: Set (ID,ID)
  , src    :: ID
  , sink   :: ID
  } deriving (Show)


data CFGData = Cond Expr
             | CFGStmt Stmt
             | ConfPoint deriving (Show)

prog1 :: Stmt
prog1 =
  Ass "x" (Var "a" `Mul` Var "b") `Comp`
  (ITE ((Var "a" `Mul` Var "b") `Gt` (Const 20 `Mul` Var "c"))
       (Ass "y" (Const 20 `Add` Var "a"))
       (Ass "y" (Const 30 `Add` Var "c"))
  ) `Comp`
  Ass "z" (Const 20 `Mul` Const 30) `Comp`
  Ass "a" (Const 20) `Comp`
  Ass "u" (Var "a" `Mul` Var "b")

prog2 :: Stmt
prog2 =
  Ass "x" (Var "a" `Mul` Var "b") `Comp` (
  ((While ((Const 20 `Mul` Var "c") `Gt` (Var "a" `Mul` Var "b"))
       ((Ass "a" (Const 20 `Add` Var "a")) `Comp`
       (Ass "c" (Var "c" `Sub` Const 1)))
  ) `Comp` (
  Ass "z" (Const 20 `Mul` Const 30) `Comp`
  Ass "a" (Const 20) `Comp`
  Ass "u" (Var "a" `Mul` Var "b"))))

cfg1 :: CFG CFGData
cfg1 = CFG {
      nodes = nodes
    , edges = S.fromList edges
    , src = 0, sink = 7
  } where
      nodes = M.fromList $ zipWith (\i a -> (i, Node i a)) [0..]
                [ CFGStmt $ Ass "x" (Var "a" `Mul` Var "b")   -- x := a * b  (0)
                , Cond ((Var "a" `Mul` Var "b") `Gt` (Const 20 `Mul` Var "c")) -- if (a * b > 20 * c) (1)
                , CFGStmt $ Ass "y" (Const 20 `Add` Var "a")  -- y := 20 + a (2)
                , CFGStmt $ Ass "y" (Const 30 `Add` Var "c")  -- y := 30 + c (3)
                , ConfPoint
                , CFGStmt $ Ass "z" (Const 20 `Mul` Const 30) -- z := 20 * 30 (4)
                , CFGStmt $ Ass "a" (Const 20)                -- a := 20 (5)
                , CFGStmt $ Ass "u" (Var "a" `Mul` Var "b")   -- u := a * b (6)
                ]
      edges = [(0,1), (1,2), (1,3), (3,4), (2,4), (4,5), (5,6), (6,7)]

cfg1' :: CFG CFGData
cfg1' = cfg prog1

cfg2 :: CFG CFGData
cfg2 = cfg prog2

type Edges      = [(ID,ID)]
type CPoints  n  = Map ID (Node n)
type NextID = ID
type CFGState a = State (NextID, [Node CFGData], Edges) a

cfg :: Stmt -> CFG CFGData
cfg s =
  let (sink,(_, ns, es)) = runState (computeCFG s) (0, [], [])
  in  CFG { nodes = fromNodes ns, edges = S.fromList $ es, src = 0, sink = sink }
    where
      fromNodes nodes = M.fromList $ map (\(Node i n) -> (i, Node i n)) nodes

-- Return value is ID of last created node
-- The state contains the ID of the *next* node!
computeCFG :: Stmt -> CFGState ID
computeCFG s' =
  case s' of
    Skip -> pred . fst' <$> get -- pred is (\x -> x - 1)
    Ass v e -> newNode (CFGStmt $ Ass v e)
    ITE e tr fl -> do
      condi <- newNode (Cond e) -- cond index
      trid <- computeCFG tr -- true index
      flid <- computeCFG fl -- false index
      newEdge condi trid
      newEdge condi flid
      confid <- newNode ConfPoint
      newEdge trid confid
      newEdge flid confid
      return confid
    Comp s1 s2 -> do
      u <- computeCFG s1
      v <- fst' <$> get
      newEdge u v
      computeCFG s2
    While e s -> do
      confid <- newNode ConfPoint
      u <- newNode (Cond e)
      newEdge confid u
      newEdge u (u+1) -- edge from condition to true branch
      v <- computeCFG s -- the end of the true branch
      newEdge v confid
      return u
  where
    fst' (a,_,_) = a
    newNode :: CFGData -> CFGState ID
    newNode n = do
      (i,ns,es) <- get
      let n' = Node i n
      put (i+1,n':ns,es)
      return $ i
    newEdge :: ID -> ID -> CFGState ()
    newEdge u v = do
      (i, ns, es) <- get
      put (i, ns, (u,v):es)

data ProgPoint =
  PP { dependent :: Set ID, node :: CFGData }
     deriving (Show)

cfgToProgP :: CFG CFGData -> [ProgPoint]
cfgToProgP (CFG {nodes, edges, src, sink}) =
  map nodeToProgP (M.elems nodes) where
    nodeToProgP node@(Node i inner) =
      let dependent = incoming i
      in  PP { dependent = dependent, node = inner }
    incoming i = S.map fst . S.filter ((i == ) . snd) $ edges


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

collectExprs :: Stmt -> Set Expr
collectExprs stmt = case stmt of
  Skip -> S.empty
  Ass _ e -> exprs e
  ITE e t f -> exprs e `union` collectExprs t `union` collectExprs f
  Comp s1 s2 -> collectExprs s1 `union` collectExprs s2
  While e s -> exprs e `union` collectExprs s


analyzeProg :: Stmt -> [Lattice]
analyzeProg prog = solveFix initial bigT where
  allExprs = collectExprs prog
  progps = cfgToProgP $ cfg prog
  bigT = eqsToBigT . progPsToEqs $ progps
  initial = replicate (length progps) allExprs
