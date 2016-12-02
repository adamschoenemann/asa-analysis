{-# LANGUAGE NamedFieldPuns #-}

module Anal.CFG where

import Data.Cmm.AST
import Data.Graph
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Functor ((<$>))
import Utils (unsafeLookup)
import Data.List (partition)
import Debug.Trace

-- control flow graph
data CFGNodeData
  = Cond Stmt
  | CFGStmt Stmt
  | ConfPoint deriving (Show)

-- could also just be Maybe Bool
data CFGEdgeData
  = NoData
  | Branch Bool
  deriving (Ord, Eq)

isBranch :: Bool -> Edge CFGEdgeData -> Bool
isBranch b (Edge _ NoData)      = False
isBranch b (Edge _ (Branch b')) = b == b'


type CFG = Graph CFGNodeData CFGEdgeData

edToMaybe :: CFGEdgeData -> Maybe Bool
edToMaybe NoData     = Nothing
edToMaybe (Branch t) = Just t

condExpr :: Stmt -> Expr
condExpr expr = case expr of
    ITE e _ _   -> e
    While e _   -> e
    Skip        -> err
    Ass _ _     -> err
    Block _     -> err
    Output _    -> err
  where err = error "only call condExpr on ifs or whiles"


instance Show CFGEdgeData where
  show NoData = ""
  show (Branch t) = show t

cfgGvzer =
  let vizNode g (Node nid x) =
        case x of
          Cond stmt -> let expr = condExpr stmt
                       in (show nid) ++ " [shape=diamond,label=\"" ++ (ppExpr expr)  ++ "\"]"
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

cfg :: [Stmt] -> CFG
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
    ite@(ITE e tr fl) -> do
      -- observation: should we just put Skip as tr and fl when we create the Cond?
      -- seems a waste keeping a reference to the branches, but maybe another
      -- analysis will need them
      condi <- newNode (Cond ite) -- cond index
      trid <- computeStmt tr -- ID of END of true branch
      flid <- computeStmt fl -- ID of END of false branch
      -- ID of START of true branch is (conditional ID) + 1
      newEdge condi (condi + 1) (Branch True)
      -- ID of START of false branch is (END of true branch) + 1
      newEdge condi (trid + 1) (Branch False)
      confid <- newNode ConfPoint
      newEdge' trid confid
      newEdge' flid confid
      return confid
    Block [] -> (pred . fst') <$> get
    Block ss -> computeBlock ss where
        computeBlock [] = (pred . fst') <$> get
        computeBlock [s1] = computeStmt s1
        computeBlock (s1:stmts) = do
            u <- computeStmt s1
            let ed = case s1 of
                          While _ _ -> Branch False
                          _         -> NoData
            v <- fst' <$> get
            newEdge u v ed
            computeBlock stmts
    w@(While e s) -> do
      confid <- newNode ConfPoint -- confluence id
      condid <- newNode (Cond w)  -- conditional id
      newEdge' confid condid
      newEdge condid (condid+1) (Branch True) -- edge from condition to true branch
      trid <- computeStmt s -- the end of the true branch
      newEdge' trid confid
      return condid
    Output e -> newNode (CFGStmt $ Output e)
  where


outgoing i = S.toList . S.map snd . S.filter ((i == ) . fst) . S.map endpoints
edgesOut i = S.toList . S.filter ((i ==) . fst . endpoints)

-- invariant: edges = [e1,e2]
-- invariant: [e1,e2] both have Branch EdgeData
-- trueFalseOut :: ID -> (ID, ID)
trueFalseOut i edges =
  let ed = edgesOut i edges
      ([tr], [fl]) = partition (isBranch True) ed
  in (snd $ endpoints tr, snd $ endpoints fl)

cfgToProgram :: CFG -> [Stmt]
cfgToProgram g@(Graph { nodes, edges, src }) =
  let source = unsafeLookup src nodes
  in  fst $ nodeToProgram source g


stmtsToStmt :: [Stmt] -> Stmt
stmtsToStmt [] = error "stmtsToStmt on empty list"
stmtsToStmt [x] = x
stmtsToStmt xs  = Block xs


nodeToProgram :: Node CFGNodeData -> CFG -> ([Stmt], ID)
nodeToProgram n g@(Graph {nodes, edges}) = help n proceed where
  getNode i = unsafeLookup i nodes

  proceed _ next = help (getNode next) proceed

  stop    i next = ([], i)

  help (Node i nd) handleConf =
    let out = outgoing i edges
    in  case (nd, out) of
          (CFGStmt stmt, [])   -> ([stmt], i)
          (CFGStmt stmt, [next]) -> stmtToProgram stmt i next handleConf
          (ConfPoint, [])        -> ([], i)
          (ConfPoint, [next])    -> handleConf i next
          (Cond (ITE b _ _), [t,f])  -> uncurry (iteToProgram b)   $ trueFalseOut i edges
          (Cond (While b _), _)  -> uncurry (whileToProgram b) $ trueFalseOut i edges

  -- stmtToProgram :: Stmt -> ID -> ID -> ([Stmt], ID)
  stmtToProgram stmt i next handleConf =
    let (stmt', i) = help (unsafeLookup next nodes) handleConf
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

