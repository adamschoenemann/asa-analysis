{-# LANGUAGE NamedFieldPuns #-}

module Avail where

import Control.Monad.Fix
import Data.Set (Set, union, (\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State
import Data.Functor ((<$>))
import Data.List (sort)

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


comp2list :: Stmt -> [Stmt]
comp2list (Comp s1 c2@(Comp s2 s3)) = s1 : comp2list c2
comp2list (Comp c1@(Comp s1 s2) s3) = comp2list c1 ++ [s3]
comp2list (Comp s1 s2) = [s1,s2]
comp2list _ = error "only flatten compositions"

type Lattice = Set Expr
type TFun = Lattice -> Lattice

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
data Node a = Node ID a deriving (Show)

data CFG a = CFG
  { nodes :: Map ID (Node a)
  , edges :: Set (ID,ID)
  , src   :: ID
  , sink  :: ID
  } deriving (Show)

data CFGNode = Cond Expr | CFGStmt Stmt deriving (Show)

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
  Ass "x" (Var "a" `Mul` Var "b") `Comp`
  (While ((Const 20 `Mul` Var "c") `Gt` (Var "a" `Mul` Var "b"))
       ((Ass "a" (Const 20 `Add` Var "a")) `Comp`
       (Ass "c" (Var "c" `Sub` Const 1)))
  ) `Comp`
  Ass "z" (Const 20 `Mul` Const 30) `Comp`
  Ass "a" (Const 20) `Comp`
  Ass "u" (Var "a" `Mul` Var "b")

cfg1 :: CFG CFGNode
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
                , CFGStmt $ Ass "z" (Const 20 `Mul` Const 30) -- z := 20 * 30 (4)
                , CFGStmt $ Ass "a" (Const 20)                -- a := 20 (5)
                , CFGStmt $ Ass "u" (Var "a" `Mul` Var "b")   -- u := a * b (6)
                ]
      edges = [(0,1), (1,2), (1,3), (3,4), (2,4), (4,5), (5,6)]

cfg1' :: CFG CFGNode
cfg1' = cfg prog1

cfg2 :: CFG CFGNode
cfg2 = cfg prog2

type Edges = [(ID,ID)]
type CFGState a = State (ID, [Node CFGNode], Edges) a

cfg :: Stmt -> CFG CFGNode
cfg s =
  let (_,(sink, ns, es)) = runState (computeCFG s) (0, [], [])
  in  CFG { nodes = fromNodes ns, edges = S.fromList $ es, src = 0, sink = sink } where
    fromNodes nodes = M.fromList $ map (\(Node i n) -> (i, Node i n)) nodes

computeCFG :: Stmt -> CFGState [ID]
computeCFG s' =
  case s' of
    Skip -> (:[]) . fst' <$> get
    Ass v e -> (:[]) <$> newNode (CFGStmt $ Ass v e)
    ITE e tr fl -> do
      condi <- newNode (Cond e) -- cond index
      -- FIXME: This will crash for nested ifs?
      trids <- computeCFG tr -- true index
      flids <- computeCFG fl -- false index
      mapM (newEdge condi) trids
      mapM (newEdge condi) flids
      return $ flids ++ trids
    Comp s1 s2 -> do
      u <- computeCFG s1
      let v = (maximum u) + 1
      mapM (flip newEdge $ v) u
      computeCFG s2
    While e s -> do
      u <- newNode (Cond e)
      newEdge u (u+1)
      v <- computeCFG s
      mapM (flip newEdge $ u) v
      return v -- return ref to cond
  where
    fst' (a,_,_) = a
    newNode :: CFGNode -> CFGState ID
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
  PP { dependent :: Set ID, node :: CFGNode }
     deriving (Show)

cfgToProgP :: CFG CFGNode -> [ProgPoint]
cfgToProgP (CFG {nodes, edges, src, sink}) =
  map nodeToProgP (M.elems nodes) where
    nodeToProgP node@(Node i inner) =
      let dependent = incoming i
      in  PP { dependent = dependent, node = inner }
    incoming i = S.map fst . S.filter ((i == ) . snd) $ edges


type Equation = [Lattice] -> Lattice

progPsToEqs :: [ProgPoint] -> [Equation]
progPsToEqs points = map pointToEq points where
  leastUpperBound :: [Lattice] -> Set ID -> Lattice
  leastUpperBound prev deps
    | S.null deps = S.empty
    | otherwise = foldl1 S.intersection . map (\d -> prev !! d) . S.toList $ deps
  pointToEq (PP { dependent, node }) prev =
    case node of
      Cond expr     -> avail expr $ leastUpperBound prev dependent
      CFGStmt stmt  -> stmtToTFun stmt $ leastUpperBound prev dependent

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