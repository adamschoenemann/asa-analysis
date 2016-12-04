
module Data.CFG where

import Data.Cmm.AST
import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Set as S
import Data.Functor ((<$>))
import Utils (unsafeLookup)
import Data.List (intercalate)
import Text.Pretty

type NodeRef = Int
type In = Int
type Out = Int
type ID = Int
type BrTrue = Int
type BrFalse = Int
type End = Int

data Node
  = Source Out
  | Single Stmt In Out
  | CondITE Expr In BrTrue BrFalse
  | CondWhile Expr In BrTrue BrFalse
  | Confluence (In, In) Out
  | Sink In
  deriving (Show, Eq)

data CFG = CFG (Map ID Node) deriving (Show, Eq)

type CFGState a = State [(ID,Node)] a

cfgToGviz :: CFG -> String
cfgToGviz (CFG m) =
  let (ns, es) = unzip . M.elems $ M.mapWithKey gviz m
      content = unlines . map ("  " ++) . filter ((0 <) . length) $ ns ++ es
  in  "digraph dg {\n" ++ content ++ "}" where
    attrs as = " [" ++ intercalate "," (map (\(a,b) -> a ++ "=\"" ++ b ++ "\"") as) ++ "]"
    label l  = ("label", l)
    gviz k n = case n of
      Source o              -> (show k ++ attrs [("shape", "point")],     show k ++ " -> " ++ show o)
      Single s i o          -> (show k ++ attrs [label (ppr s), ("shape", "box")], show k ++ " -> " ++ show o)
      CondITE e i bt bf     -> (show k ++ attrs [label (ppr e), ("shape","diamond")],
                                show k ++ " -> " ++ show bt ++ "[label=\"True\"]\n  " ++ show k ++ " -> " ++ show bf ++ "[label=\"False\"]")
      CondWhile e i bt bf   -> (show k ++ attrs [label (ppr e), ("shape", "diamond")],
                                show k ++ " -> " ++ show bt ++ "[label=\"True\"]\n  " ++ show k ++ " -> " ++ show bf ++ "[label=\"False\"]")
      Confluence (i1, i2) o -> (show k ++ attrs [("shape", "circle")],show k ++ " -> " ++ show o)
      Sink i                -> (show k ++ attrs [("shape", "point")], "") -- show k


progToCfg :: [Stmt] -> CFG
progToCfg stmts =
  let ((i, fn), assoc) = runState (cfgStmt 1 $ Block stmts) []
      (j,last) = fn (i+1)
      graph = (0, Source 1) : (assoc ++ [(i,last),(i+1, Sink i)])
  in CFG $ M.fromList graph

-- Takes an int representing the "root" ID
-- and a statement
-- returns a computation that will generate a sub-graph (a list of nodes)
-- representing the control-flow of the statement *except* the last node
-- the result is the "requested" ID of the last node, and a function that
-- given an ID j will create the last node of the sub-graph with j as its
-- outgoing edge. The node is *not* added to the graph - it is the responsibility
-- of the caller to add it using e.g. newNode
cfgStmt :: Int -> Stmt -> CFGState (End, Out -> (ID, Node))
cfgStmt i stmt = case stmt of
  Skip     -> return $ (i, \j -> (i, Single Skip (i-1) j))
  Ass v e  -> return $ (i, \j -> (i, Single (Ass v e) (i-1) j))
  Output e -> return $ (i, \j -> (i, Single (Output e) (i-1) j))
  Block [] -> return $ (i, \j -> (i, error "cannot handle empty block"))
  Block [s] -> cfgStmt i s
  Block (s:ss) -> do
    (j, b) <- cfgStmt i s
    let (k, n) = b (succ j)
    newNode k $ n
    cfgStmt (succ j) $ Block ss
  ITE e tr fl -> do
    (trid, trb) <- cfgStmt (i+1) tr
    (flid, flb) <- cfgStmt (trid+1) fl
    newNode i $ CondITE e (i-1) (i+1) (trid + 1)
    let fun = \j -> (flid+1, Confluence (trid, flid) j)
    newNode trid $ snd $ trb (flid + 1)
    newNode flid $ snd $ flb (flid + 1)
    return (flid + 1, fun)
  While e tr -> do
    (trid, trb) <- cfgStmt (i+2) tr -- create true branch
    newNode trid $ snd $ trb i -- make end of true branch point to confluence
    newNode i     $ Confluence (i-1, trid) (i+1) -- create confluence
    let fun = \j -> (i+1,CondWhile e i (i+2) (trid+1)) -- create conditional node
    return (trid, fun)


  s -> error $ "unexpected " ++ show s

newNode :: Int -> Node -> CFGState ()
newNode i n = modify ((i,n):)

