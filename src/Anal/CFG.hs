
module Anal.CFG where

import Data.Cmm.AST
import Data.Graph
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Functor ((<$>))

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

type Edges      = [Edge CFGEdgeData]
type NextID = ID
type CFGState a = State (NextID, [Node CFGNodeData], Edges) a

cfg :: [Stmt] -> Graph CFGNodeData CFGEdgeData
cfg ss =
  let (sink,(_, ns, es)) = runState (computeGraph ss) (0, [], [])
  in  Graph { nodes = fromNodes ns, edges = S.fromList $ es, src = 0, sink = sink }
    where
      fromNodes nodes = M.fromList $ map (\(Node i n) -> (i, Node i n)) nodes

computeGraph :: [Stmt] -> CFGState ID
computeGraph stmts = computeStmt (Block stmts)

fst' :: (a,b,c) -> a
fst' (a,_,_) = a

newEdge :: ID -> ID -> CFGEdgeData -> CFGState ()
newEdge u v ed = do
  (i, ns, es) <- get
  let edge = Edge (u,v) ed
  put (i, ns, edge:es)

newEdge' :: ID -> ID -> CFGState ()
newEdge' u v = newEdge u v NoData

newNode :: CFGNodeData -> CFGState ID
newNode n = do
  (i,ns,es) <- get
  let n' = Node i n
  put (i+1,n':ns,es)
  return $ i


-- Return value is ID of last created node
-- The state contains the ID of the *next* node!
computeStmt :: Stmt -> CFGState ID
computeStmt s' =
  case s' of
    Skip -> newNode (CFGStmt Skip) --pred . fst' <$> get -- pred is (\x -> x - 1)
    Ass v e -> newNode (CFGStmt $ Ass v e)
    ITE e tr fl -> do
      condi <- newNode (Cond e) -- cond index
      trid <- computeStmt tr -- true index
      flid <- computeStmt fl -- false index
      newEdge condi (condi + 1) (Branch True)
      newEdge condi flid (Branch False)
      confid <- newNode ConfPoint
      newEdge' trid confid
      newEdge' flid confid
      return confid
    Block [] -> fst' <$> get
    Block ss -> computeBlock ss where
        computeBlock [] = fst' <$> get
        computeBlock [s1] = computeStmt s1
        computeBlock (s1:stmts) = do
            u <- computeStmt s1
            let ed = case s1 of
                          While _ _ -> Branch False
                          _         -> NoData
            v <- fst' <$> get
            newEdge u v ed
            computeBlock stmts
    While e s -> do
      confid <- newNode ConfPoint -- confluence id
      condid <- newNode (Cond e)  -- conditional id
      newEdge' confid condid
      newEdge condid (condid+1) (Branch True) -- edge from condition to true branch
      trid <- computeStmt s -- the end of the true branch
      newEdge' trid confid
      return condid
    Output e -> newNode (CFGStmt $ Output e)
  where
