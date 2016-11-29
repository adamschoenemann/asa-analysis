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
  IConst _    -> S.empty
  BConst _    -> S.empty
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
    IConst _  -> False
    BConst _  -> False
    Var s     -> s == v
    Input     -> False

assign :: String -> Expr -> Lattice -> Lattice
assign v e  = avail e . unavail v

stmtToTFun :: Stmt -> TFun
stmtToTFun stmt = case stmt of
  Skip -> id
  Ass v e -> assign v e
  ITE e s s' -> avail e
  Comp s s' -> id
  While e s -> avail e

-- control flow graph
data CFGNodeData
  = Cond Expr
  | CFGStmt Stmt
  | ConfPoint deriving (Show)

-- could also just be Maybe Bool
data CFGEdgeData
  = NoData
  | Branch Bool
  deriving (Ord, Eq)

edToMaybe :: CFGEdgeData -> Maybe Bool
edToMaybe NoData     = Nothing
edToMaybe (Branch t) = Just t

instance Show CFGEdgeData where
  show NoData = ""
  show (Branch t) = show t

cfgGvzer =
  let vizNode g (Node nid x) =
        case x of
          Cond exp -> (show nid) ++ " [shape=diamond,label=\"" ++ (ppExpr exp)  ++ "\"]"
          CFGStmt stmt -> (show nid) ++ " [shape=box,label=\"" ++ (ppStmt 0 stmt) ++ "\"]"
          ConfPoint -> show nid
      vizEdge g (Edge (u,v) ed) =
        let attrs = maybe "" (\t -> " [label=\"" ++ (show t) ++ "\"]") (edToMaybe ed)
        in  show u ++ " -> " ++ show v ++ attrs
  in Graphvizer { vizNode = vizNode, vizEdge = vizEdge }


vizCfg g name = toGraphviz g name cfgGvzer
writeVizCfg g name = writeFile ("./graphviz/" ++ name ++ ".dot") (vizCfg g name)


prog1 :: Stmt
prog1 =
  Ass "x" (Var "a" `Mul` Var "b") `Comp`
  (ITE ((Var "a" `Mul` Var "b") `Gt` (IConst 20 `Mul` Var "c"))
       (Ass "y" (IConst 20 `Add` Var "a"))
       (Ass "y" (IConst 30 `Add` Var "c"))
  ) `Comp`
  Ass "z" (IConst 20 `Mul` IConst 30) `Comp`
  Ass "a" (IConst 20) `Comp`
  Ass "u" (Var "a" `Mul` Var "b")

prog2 :: Stmt
prog2 =
  Ass "x" (Var "a" `Mul` Var "b") `Comp` (
  ((While ((IConst 20 `Mul` Var "c") `Gt` (Var "a" `Mul` Var "b"))
       ((Ass "a" (IConst 20 `Add` Var "a")) `Comp`
       (Ass "c" (Var "c" `Sub` IConst 1)))
  ) `Comp` (
  Ass "z" (IConst 20 `Mul` IConst 30) `Comp`
  Ass "a" (IConst 20) `Comp`
  Ass "u" (Var "a" `Mul` Var "b"))))


cfg1 :: Graph CFGNodeData CFGEdgeData
cfg1 = Graph {
      nodes = nodes
    , edges = S.fromList $ map (\(u,v,d) -> Edge (u,v) d) edges
    , src = 0, sink = 7
  } where
      nodes = M.fromList $ zipWith (\i a -> (i, Node i a)) [0..]
                [ CFGStmt $ Ass "x" (Var "a" `Mul` Var "b")   -- x := a * b  (0)
                , Cond ((Var "a" `Mul` Var "b") `Gt` (IConst 20 `Mul` Var "c")) -- if (a * b > 20 * c) (1)
                , CFGStmt $ Ass "y" (IConst 20 `Add` Var "a")  -- y := 20 + a (2)
                , CFGStmt $ Ass "y" (IConst 30 `Add` Var "c")  -- y := 30 + c (3)
                , ConfPoint
                , CFGStmt $ Ass "z" (IConst 20 `Mul` IConst 30) -- z := 20 * 30 (4)
                , CFGStmt $ Ass "a" (IConst 20)                -- a := 20 (5)
                , CFGStmt $ Ass "u" (Var "a" `Mul` Var "b")   -- u := a * b (6)
                ]
      no = NoData
      edges = [(0,1,no), (1,2, Branch True), (1,3, Branch False), (3,4,no), (2,4,no), (4,5,no), (5,6,no), (6,7,no)]

cfg1' :: Graph CFGNodeData CFGEdgeData
cfg1' = cfg prog1

cfg2 :: Graph CFGNodeData CFGEdgeData
cfg2 = cfg prog2

type Edges      = [Edge CFGEdgeData]
type CPoints  n  = Map ID (Node n)
type NextID = ID
type CFGState a = State (NextID, [Node CFGNodeData], Edges) a

cfg :: Stmt -> Graph CFGNodeData CFGEdgeData
cfg s =
  let (sink,(_, ns, es)) = runState (computeGraph s) (0, [], [])
  in  Graph { nodes = fromNodes ns, edges = S.fromList $ es, src = 0, sink = sink }
    where
      fromNodes nodes = M.fromList $ map (\(Node i n) -> (i, Node i n)) nodes

-- Return value is ID of last created node
-- The state contains the ID of the *next* node!
computeGraph :: Stmt -> CFGState ID
computeGraph s' =
  case s' of
    Skip -> pred . fst' <$> get -- pred is (\x -> x - 1)
    Ass v e -> newNode (CFGStmt $ Ass v e)
    ITE e tr fl -> do
      condi <- newNode (Cond e) -- cond index
      trid <- computeGraph tr -- true index
      flid <- computeGraph fl -- false index
      newEdge condi trid (Branch True)
      newEdge condi flid (Branch False)
      confid <- newNode ConfPoint
      newEdge' trid confid
      newEdge' flid confid
      return confid
    Comp s1 s2 -> do
      u <- computeGraph s1
      let ed = case s1 of
                    While _ _ -> Branch False
                    _         -> NoData
      v <- fst' <$> get
      newEdge u v ed
      computeGraph s2
    While e s -> do
      confid <- newNode ConfPoint -- confluence id
      condid <- newNode (Cond e)  -- conditional id
      newEdge' confid condid
      newEdge condid (condid+1) (Branch True) -- edge from condition to true branch
      trid <- computeGraph s -- the end of the true branch
      newEdge' trid confid
      return condid
  where
    fst' (a,_,_) = a
    newNode :: CFGNodeData -> CFGState ID
    newNode n = do
      (i,ns,es) <- get
      let n' = Node i n
      put (i+1,n':ns,es)
      return $ i
    newEdge :: ID -> ID -> CFGEdgeData -> CFGState ()
    newEdge u v ed = do
      (i, ns, es) <- get
      let edge = Edge (u,v) ed
      put (i, ns, edge:es)
    newEdge' u v = newEdge u v NoData

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
