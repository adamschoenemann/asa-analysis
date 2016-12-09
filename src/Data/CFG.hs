{-# LANGUAGE DeriveGeneric #-}
module Data.CFG where

import GHC.Generics (Generic)
import Data.Cmm.AST
import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Functor ((<$>))
import Utils (unsafeLookup)
import Data.List (intercalate)
import Text.Pretty
import Debug.Trace
import Control.DeepSeq

type ID = Int
type In = ID
type Out = ID
type BrTrue = ID
type BrFalse = ID
type End = ID

data Node
  = Source Out
  | Single Stmt In Out
  | CondITE Expr In BrTrue BrFalse End -- end of the conditional (points to confluence)
  | CondWhile Expr In BrTrue BrFalse End -- end of the conditional (points to confluence)
  | Confluence (In, In) Out
  | Sink In
  deriving (Show, Eq, Generic)

instance NFData Node

getOutgoing :: Node -> [ID]
getOutgoing node = case node of
  Source o              -> [o]
  Single s i o          -> [o]
  CondITE e i bt bf c   -> [bt,bf]
  CondWhile e i bt bf c -> [bt,bf]
  Confluence (i1, i2) o -> [o]
  Sink i                -> []

data CFG = CFG (Map ID Node) deriving (Show, Eq, Generic)

instance NFData CFG where

type CFGState a = State [(ID,Node)] a

cfgToGviz :: String -> CFG -> String
cfgToGviz name (CFG m) =
  let (ns, es) = unzip . M.elems $ M.mapWithKey gviz m
      content = unlines . map ("  " ++) . filter ((0 <) . length) $ ns ++ es
  in  "digraph " ++ name ++ " {\n" ++ content ++ "}" where
    attrs as = " [" ++ intercalate "," (map (\(a,b) -> a ++ "=\"" ++ b ++ "\"") as) ++ "]"
    label l  = ("label", l)
    backedge k o = "" -- "\n  " ++ show k ++ " -> " ++ show o ++ " [style=dashed]"
    gviz k n = case n of
      Source o              -> (show k ++ attrs [("shape", "point")],     show k ++ " -> " ++ show o)
      Single s i o          -> (show k ++ attrs [label (ppr s), ("shape", "box")],
                                show k ++ " -> " ++ show o ++ backedge k i)
      CondITE e i bt bf _   -> (show k ++ attrs [label (ppr e), ("shape","diamond")],
                                show k ++ " -> " ++ show bt ++ "[label=\"True\"]\n  " ++ show k ++ " -> " ++ show bf ++ "[label=\"False\"]"
                                ++ backedge k i)
      CondWhile e i bt bf _ -> (show k ++ attrs [label (ppr e), ("shape", "diamond")],
                                show k ++ " -> " ++ show bt ++ "[label=\"True\"]\n  " ++ show k ++ " -> " ++ show bf ++ "[label=\"False\"]"
                                ++ backedge k i)
      Confluence (i1, i2) o -> (show k ++ attrs [("shape", "circle")],show k ++ " -> " ++ show o ++ backedge k i1 ++ backedge k i2)
      Sink i                -> (show k ++ attrs [("shape", "point")], backedge k i) -- show k

writeVizCfg :: CFG -> String -> IO ()
writeVizCfg g name = writeFile ("./graphviz/" ++ name ++ ".dot") (cfgToGviz name g)

progToCfg :: [Stmt] -> CFG
progToCfg stmts =
  let ((i, fn), assoc') = runState (cfgStmt 0 1 $ Block stmts) []
      (_, assoc)      = runState (fn (i+1)) assoc'
      graph = (0, Source 1) : (assoc ++ [(i+1, Sink i)])
  in CFG $ M.fromList graph

-- Takes an int representing the "root" ID
-- and a statement
-- returns a computation that will generate a sub-graph (a list of nodes)
-- representing the control-flow of the statement *except* the last node
-- the result is the "requested" ID of the last node, and a function that
-- given an ID j will create the last node of the sub-graph with j as its
-- outgoing edge.
cfgStmt :: In -> Int -> Stmt -> CFGState (End, Out -> CFGState ID)
cfgStmt p i stmt = case stmt of
  Skip     -> return $ (i, \j -> newNode i $ Single Skip p j)
  Ass v e  -> return $ (i, \j -> newNode i $ Single (Ass v e) p j)
  Output e -> return $ (i, \j -> newNode i $ Single (Output e) p j)
  Block [] -> return $ (i, \j -> return i)
  Block [s] -> cfgStmt p i s
  Block (s:ss) -> do
    (j, b) <- cfgStmt p i s
    p <- b (succ j)
    cfgStmt p (succ j) $ Block ss
  ITE e tr fl -> do
    (trid, trb) <- cfgStmt i (i+1)    tr
    (flid, flb) <- cfgStmt i (trid+1) fl
    _ <- newNode i $ CondITE e (i-1) (i+1) (trid+1) (flid+1)
    let fun = \j -> newNode (flid+1) $ Confluence (trid, flid) j
    _ <- trb (flid + 1)
    _ <- flb (flid + 1)
    return (flid + 1, fun)
    {-
                      <i>
                    /     \
                [i+1]    [trid+1]
                  |         |
                [trid]   [flid]
                    \     /
                   (flid+1)
    -}
  While e tr -> do
    (trid, trb) <- cfgStmt (i+1) (i+2) tr -- create true branch
    _ <- trb i -- make end of true branch point to confluence
    _ <- newNode i     $ Confluence (i-1, trid) (i+1) -- create confluence
    let fun = \j -> newNode (i+1) $ CondWhile e i (i+2) (trid+1) i -- create conditional node
    return (trid, fun)

newNode :: Int -> Node -> CFGState ID
newNode i n = modify ((i,n):) >> return i

cfgToProgram :: CFG -> [Stmt]
cfgToProgram g@(CFG nodes) =
  let source = unsafeLookup 0 nodes
  in  fst $ nodeToProgram (0,source) g

nodeToProgram :: (ID,Node) -> CFG -> ([Stmt], ID)
nodeToProgram n (CFG nodes) = help S.empty n proceed where
  getNode :: ID -> (ID, Node)
  getNode i = (i,unsafeLookup i nodes)

  proceed ex _ next  = help ex (getNode next) proceed
  stop   _ i next  = ([], i)
  stopIf j ex i next = if j == i then ([], i) else help ex (getNode next) (stopIf j)

  help :: Set ID -> (ID, Node) -> (Set ID -> Int -> Int -> ([Stmt], ID)) -> ([Stmt], ID)
  help ex (i, nd) handleConf
    | i `S.member` ex = ([], i)
    | otherwise =
        let ex' = S.insert i ex
        in case nd of
          Source o              -> help ex' (getNode o) handleConf
          Single s i o          -> stmtToProgram ex' s i o handleConf
          CondITE e i bt bf c   -> iteToProgram ex' e bt bf c
          CondWhile e i bt bf c -> whileToProgram ex' e bt bf c
          Confluence (i1, i2) o -> handleConf ex' i o
          Sink i                -> ([], i)

  stmtToProgram ex stmt i next handleConf =
    let (stmt', i) = help ex (getNode next) handleConf
    in  (stmt:stmt', i)

  whileToProgram :: Set ID -> Expr -> In -> ID -> ID -> ([Stmt], ID)
  whileToProgram ex b trid flid c =
    let (tr, _) = help ex (getNode trid) (stopIf c)
        (fl, i)   = help ex (getNode flid) stop
    in (While b (stmtsToStmt tr) : fl, i)

  iteToProgram :: Set ID -> Expr -> ID -> ID -> ID -> ([Stmt], ID)
  iteToProgram ex b trid flid c =
    let (tr,tcid)  = help ex (getNode trid) (stopIf c) -- (branch, true confluence id)
        (fl,fcid)  = help ex (getNode flid) (stopIf c) -- (brancid)
        continue =
            let ite = ITE b (stmtsToStmt tr) (stmtsToStmt fl)
                (stmts, i) = help ex (getNode tcid) proceed
            in  (ite:stmts, i)
    in  assert (tcid /= fcid) ("confluence ids do not match. t: " ++ show tcid ++ ", f: " ++ show fcid) continue

  assert b e c = if b then (error e) else c


-- depth first traversal
dfTraverseCFG :: CFG -> (a -> Node -> a) -> a -> a
dfTraverseCFG (CFG nodes) fn start =
  let src = getNode 0
  in  help S.empty start src where
    help explored acc (i,node)
      | i `S.member` explored = acc
      | otherwise =
        let acc' = fn acc node
            explored' = S.insert i explored
            out = map getNode $ getOutgoing node
            folder old next = help explored' old next
        in foldl folder acc' out
    getNode i = (i,unsafeLookup i nodes)


data DFAlg a =
  DFAlg { dfWhile  :: ID -> Expr -> a -> a -> a
        , dfITE    :: ID -> Expr -> a -> a -> a -> a
        , dfSingle :: ID -> Stmt -> a -> a
        }

dfFoldCFGAlg :: DFAlg a -> CFG -> a -> a
dfFoldCFGAlg (DFAlg while ite single) (CFG nodes) start =
  let src = getNode 0
  in  help S.empty start src proceed where

    help explored acc (i,node) confluence
      | i `S.member` explored = acc
      | otherwise =
        let explored' = S.insert i explored
        in case node of
          Source o              -> help explored' acc (getNode o) confluence
          Single s _ o          -> single i s (help explored' acc (getNode o) confluence)
          CondITE e _ bt bf c   ->
            let trb  = help explored' acc (getNode bt) (stopAt c)
                flb  = help explored' acc (getNode bf) (stopAt c)
                (_,Confluence _ o) = getNode c
            in  ite i e trb flb (help explored' acc (getNode o) proceed)
          CondWhile e _ bt bf c ->
            let trb = help explored' acc (getNode bt) (stopAt c)
                flb = help explored' acc (getNode bf) confluence
            in  while i e trb flb
          Confluence (i1, i2) o -> confluence explored' i o acc
          Sink _                -> acc

    proceed ex  i' n acc  = help ex acc (getNode n) proceed
    stopAt j ex i' n acc
      | j == i' = acc
      | otherwise = help ex acc (getNode n) (stopAt j)

    getNode i = (i,unsafeLookup i nodes)