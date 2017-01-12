{-
Module for representing control-flow graphs (CFG)
Its not so easy :)
-}
{-# LANGUAGE DeriveGeneric, ScopedTypeVariables #-}
module Data.CFG where

import GHC.Generics (Generic)
import Data.Cmm.AST
import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as S
import Utils (unsafeLookup)
import Data.List (intercalate)
import Text.Pretty
import Control.DeepSeq

-- Some type aliases just for documentation purposes
-- in the Node AST
type ID = Int
type In = ID
type Out = ID
type BrTrue = ID
type BrFalse = ID
type End = ID

-- A node in the CFG graph
data Node
  = NSource Out          -- the source node
  | NSingle Stmt In Out  -- a single statement (box)
  -- An ITE node, with 1 incoming, 2 outgoing (true and false)
  -- The End ID  points to the end of the conditional (a confluence)
  -- its not really an edge but just to keep track of where the branches end
  | NITE Expr In BrTrue BrFalse End
  -- A While node, with the same hack as the ITE
  | NWhile Expr In BrTrue BrFalse End -- end of the conditional (points to confluence)
  -- A confluence with two incoming and one outgoing
  | NConfl (In, In) Out
  -- A sink just has one incoming
  | NSink In
  deriving (Show, Eq, Generic)

-- This is just for testing stuff with DeepSeq
instance NFData Node

-- get the outgoing edges of a node as a list
getOutgoing :: Node -> [ID]
getOutgoing node = case node of
  NSource o          -> [o]
  NSingle s i o      -> [o]
  NITE e i bt bf c   -> [bt,bf]
  NWhile e i bt bf c -> [bt,bf]
  NConfl (i1, i2) o  -> [o]
  NSink i            -> []

-- A control-flow graph is simply a map from ID to Node
data CFG = CFG (Map ID Node) deriving (Show, Eq, Generic)

-- just for DeepSeq testing
instance NFData CFG where

-- alias for a stateful computation with CFG's
type CFGState a = State [(ID,Node)] a

-- print a CFG as a graph-viz representation.
-- for nice debugging
-- not pretty but it works
cfgToGviz :: String -> CFG -> String
cfgToGviz name (CFG m) =
  let (ns, es) = unzip . M.elems $ M.mapWithKey gviz m
      content = unlines . map ("  " ++) . filter ((0 <) . length) $ ns ++ es
  in  "digraph " ++ name ++ " {\n" ++ content ++ "}" where
    attrs as = " [" ++ intercalate "," (map (\(a,b) -> a ++ "=\"" ++ b ++ "\"") as) ++ "]"
    label l  = ("label", l)
    backedge k o = "" -- "\n  " ++ show k ++ " -> " ++ show o ++ " [style=dashed]"
    gviz k n = case n of
      NSource o           -> (show k ++ attrs [("shape", "point")],     show k ++ " -> " ++ show o)
      NSingle s i o       -> (show k ++ attrs [label (ppr s), ("shape", "box")],
                              show k ++ " -> " ++ show o ++ backedge k i)
      NITE e i bt bf _    -> (show k ++ attrs [label (ppr e), ("shape","diamond")],
                              show k ++ " -> " ++ show bt ++ "[label=\"True\"]\n  "
                              ++ show k ++ " -> " ++ show bf ++ "[label=\"False\"]"
                              ++ backedge k i)
      NWhile e i bt bf _  -> (show k ++ attrs [label (ppr e), ("shape", "diamond")],
                              show k ++ " -> " ++ show bt ++ "[label=\"True\"]\n  "
                              ++ show k ++ " -> " ++ show bf ++ "[label=\"False\"]"
                              ++ backedge k i)
      NConfl (i1, i2) o   -> (show k ++ attrs [("shape", "circle")],show k ++ " -> "
                              ++ show o ++ backedge k i1 ++ backedge k i2)
      NSink i             -> (show k ++ attrs [("shape", "point")], backedge k i)

-- gviz cfg and write to file
writeVizCfg :: CFG -> String -> IO ()
writeVizCfg g name = writeFile ("./graphviz/" ++ name ++ ".dot") (cfgToGviz name g)

-- convert a program to a CFG!
-- mainly calls the stateful `cfgSubProg` function
progToCfg :: Program -> CFG
progToCfg stmts =
  let ((i, fn), assoc') = runState (cfgSubProg 0 1 $ Block stmts) []
      (_, assoc)        = runState (fn (i+1)) assoc'
      graph = (0, NSource 1) : (assoc ++ [(i+1, NSink i)])
  in CFG $ M.fromList graph

type Next = ID
-- Takes an int representing the "root" ID and an int representing the
-- next ID, i.e. the ID for the next node to be created
-- and a statement
-- returns a computation that will generate a sub-graph (a list of nodes)
-- representing the control-flow of the statement *except* the last node.
-- the result is the "requested" ID of the last node, and a function that
-- given an ID j will create the last node of the sub-graph with j as its
-- outgoing edge.
cfgSubProg :: In -> Next -> SubProg -> CFGState (End, Out -> CFGState ID)
cfgSubProg p i stmt = case stmt of
  Single Skip       -> return $ (i, \j -> newNode i $ NSingle Skip p j)
  Single (Ass v e ) -> return $ (i, \j -> newNode i $ NSingle (Ass v e) p j)
  Single (Output e) -> return $ (i, \j -> newNode i $ NSingle (Output e) p j)
  Block [] -> return $ (i, \j -> return i)
  Block [s] -> cfgSubProg p i s
  Block (s:ss) -> do
    (j, b) <- cfgSubProg p i s
    p <- b (succ j)
    cfgSubProg p (succ j) $ Block ss
  ITE e tr fl -> do
    let trbroot = i + 1
    (trid, trb) <- cfgSubProg i trbroot tr
    let flbroot = trid + 1
    (flid, flb) <- cfgSubProg i flbroot fl
    let conflid = flid + 1
    -- create the conditional node (diamond)
    _ <- newNode i $ NITE e (i-1) trbroot flbroot conflid
    let fun = \j -> newNode conflid $ NConfl (trid, flid) j
    _ <- trb conflid
    _ <- flb conflid
    return (conflid, fun)
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
    {-
            +------->(i)
            |         |
            |       <i+1>
            |     /      \
            |   [i+2]  [trid+1]
            |     |       |
            +--[trid]
    -}
    (trid, trb) <- cfgSubProg (i+1) (i+2) tr -- create true branch
    _ <- trb i -- make end of true branch point to confluence
    _ <- newNode i     $ NConfl (i-1, trid) (i+1) -- create confluence
    let fun = \j -> newNode (i+1) $ NWhile e i (i+2) (trid+1) i -- create conditional node
    return (trid, fun)

-- create a new node with and ID and return
-- the ID we created it with
newNode :: ID -> Node -> CFGState ID
newNode i n = modify ((i,n):) >> return i


-- an algebra for "folding" over CFGs in a depth-first traversal
data DFAlg a =
  DFAlg { dfWhile   :: ID -> Expr -> a -> a -> a
        , dfITE     :: ID -> Expr -> a -> a -> a -> a
        , dfNSingle :: ID -> Stmt -> a -> a
        }

-- fold depth-first over a CFG with an algebra
dfFoldCFGAlg :: forall a. DFAlg a -> CFG -> a -> a
dfFoldCFGAlg (DFAlg while ite single) (CFG nodes) initial =
  let src = getNode 0 -- get source
  -- begin with an empty explored set
  -- proceed is also a kind of ConfluenceHandler
  in  help S.empty initial src proceed where

    -- 1: the explored list
    -- 2: the accumulator
    -- 3: a (ID, Node)
    -- 4: a "confluence-helper" (not the type alias though)
    -- returns an a
    help :: Set ID -> a -> (ID, Node) -> (Set ID -> ID -> ID -> a -> a) -> a
    help explored acc (i,node) handleConf
      | i `S.member` explored = acc -- already explored, so quit
      | otherwise =
        let explored' = S.insert i explored -- insert i into explored
        in case node of
          -- continue from o if Source. There is nothing to fold over inside a Source
          NSource o          -> help explored' acc (getNode o) handleConf
          -- call `single` with i s and the result of continuing from o
          NSingle s _ o      -> single i s (help explored' acc (getNode o) handleConf)
          NITE e _ bt bf c   ->
            let -- continue from true-branch
                trb  = help explored' acc (getNode bt) (stopAt c)
                -- continue from false-branch
                flb  = help explored' acc (getNode bf) (stopAt c)
                -- get the outgoing edge of the confluence node
                (_,NConfl _ o) = getNode c
                -- call `ite` with i e trb flb and continue from the confluence
            in  ite i e trb flb (help explored' acc (getNode o) proceed)
          NWhile e _ bt bf c ->
            let -- continue with true-branch
                trb = help explored' acc (getNode bt) (stopAt c)
                -- continue with false-branch
                flb = help explored' acc (getNode bf) handleConf
                -- combine with while
            in  while i e trb flb
          -- use handleConf to quit or continue at this node
          NConfl (i1, i2) o -> handleConf explored' i o acc
          -- we're done!
          NSink _           -> acc

    -- proceed no matter what when we encounter a confluence node
    proceed ex  i' n acc  = help ex acc (getNode n) proceed

    -- stop if we reach confluence with id=j
    stopAt j ex i' n acc
      | j == i' = acc
      | otherwise = help ex acc (getNode n) (stopAt j)

    getNode i = (i,unsafeLookup i nodes)

-- convert a cfg to a program
cfgToProgram :: CFG -> Program
cfgToProgram cfg = dfFoldCFGAlg alg cfg [] where
  alg = DFAlg { dfWhile=while, dfITE=ite, dfNSingle=single }
  while i e t f   = While e (stmtsToSubProg t) : f
  ite i e t f n   = ITE e (stmtsToSubProg t) (stmtsToSubProg f) :n
  single i stmt n = (Single stmt) : n