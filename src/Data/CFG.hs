
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
import Debug.Trace

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

cfgToGviz :: String -> CFG -> String
cfgToGviz name (CFG m) =
  let (ns, es) = unzip . M.elems $ M.mapWithKey gviz m
      content = unlines . map ("  " ++) . filter ((0 <) . length) $ ns ++ es
  in  "digraph " ++ name ++ " {\n" ++ content ++ "}" where
    attrs as = " [" ++ intercalate "," (map (\(a,b) -> a ++ "=\"" ++ b ++ "\"") as) ++ "]"
    label l  = ("label", l)
    backedge k o = "\n  " ++ show k ++ " -> " ++ show o ++ " [style=dashed]"
    gviz k n = case n of
      Source o              -> (show k ++ attrs [("shape", "point")],     show k ++ " -> " ++ show o)
      Single s i o          -> (show k ++ attrs [label (ppr s), ("shape", "box")],
                                show k ++ " -> " ++ show o ++ backedge k i)
      CondITE e i bt bf     -> (show k ++ attrs [label (ppr e), ("shape","diamond")],
                                show k ++ " -> " ++ show bt ++ "[label=\"True\"]\n  " ++ show k ++ " -> " ++ show bf ++ "[label=\"False\"]"
                                ++ backedge k i)
      CondWhile e i bt bf   -> (show k ++ attrs [label (ppr e), ("shape", "diamond")],
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
    _ <- newNode i $ CondITE e (i-1) (i+1) (trid+1)
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
    let fun = \j -> newNode (i+1) $ CondWhile e i (i+2) (trid+1) -- create conditional node
    return (trid, fun)
  s -> error $ "unexpected " ++ show s

newNode :: Int -> Node -> CFGState ID
newNode i n = modify ((i,n):) >> return i

cfgToProgram :: CFG -> [Stmt]
cfgToProgram g@(CFG nodes) =
  let source = unsafeLookup 0 nodes
  in  fst $ nodeToProgram (0,source) g

nodeToProgram :: (Int,Node) -> CFG -> ([Stmt], ID)
nodeToProgram n (CFG nodes) = help n proceed where
  getNode :: ID -> (ID, Node)
  getNode i = (i,unsafeLookup i nodes)
  proceed _ next = help (getNode next) proceed
  stop    i next = ([], i)

  help :: (ID, Node) -> (Int -> Int -> ([Stmt], ID)) -> ([Stmt], ID)
  help (i, nd) handleConf =
    case nd of
      Source o              -> help (getNode o) handleConf
      Single s i o          -> stmtToProgram s i o handleConf
      CondITE e i bt bf     -> iteToProgram e bt bf
      CondWhile e i bt bf   -> whileToProgram e bt bf
      Confluence (i1, i2) o -> handleConf i o
      Sink i                -> ([], i)

  stmtToProgram stmt i next handleConf =
    let (stmt', i) = help (getNode next) handleConf
    in  (stmt:stmt', i)

  whileToProgram :: Expr -> ID -> ID -> ([Stmt], ID)
  whileToProgram b trid flid =
    let (tr, _) = help (getNode trid) stop
        (fl, i)   = help (getNode flid) proceed
    in (While b (stmtsToStmt tr) : fl, i)

  iteToProgram :: Expr -> ID -> ID -> ([Stmt], ID)
  iteToProgram b trid flid =
    let (tr,tcid)  = help (getNode trid) stop -- (branch, true confluence id)
        (fl,fcid)  = help (getNode flid) stop -- (branch, false confluence id)
    in if (tcid /= fcid)
      then error "confluence ids do not match"
      else
        let ite = ITE b (stmtsToStmt tr) (stmtsToStmt fl)
            (stmts, i) = help (getNode tcid) proceed
        in  (ite:stmts, i)

  stmtsToStmt :: [Stmt] -> Stmt
  stmtsToStmt [] = error "stmtsToStmt on empty list"
  stmtsToStmt [x] = x
  stmtsToStmt xs  = Block xs