{-# LANGUAGE NamedFieldPuns, FlexibleInstances, GADTs, StandaloneDeriving #-}

module Anal where

import Data.Set (Set, union, (\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Cmm.AST
import Data.CFG
import Data.ProgPoint
import Text.Pretty
import Utils
import Control.Monad.State (runState, get, modify, State)
import Debug.Trace
import Data.Functor ((<$>))

class (Ord a, Eq a) => Lat a where
  bottom :: a
  leastUpperBound :: a -> a -> a

-- Transfer Function
type TFun a = a -> a

-- a function from dependencies to a single lattice element
type Equation a = [a] -> a


data Analysis a where
  Analysis :: Lat a => { stmtToTFun :: Stmt -> TFun a
                       , condToTFun :: Expr -> TFun a
                       , initialEnv :: [Stmt] -> a
                       , firstPPEnv :: a -- first program point environment
                       } -> Analysis a

progPsToEqs :: Lat a => Analysis a -> [ProgPoint] -> [Equation a]
progPsToEqs anal points = map pointToEq points where
  pointToEq (PP dependent (k, node)) prev =
    case node of
      Source o              -> firstPPEnv anal
      Single stmt i o       -> (stmtToTFun anal $ stmt) (prev !! i)
      CondITE e i bt bf _   -> (condToTFun anal $ e)    (prev !! i)
      CondWhile e i bt bf _ -> (condToTFun anal $ e)    (prev !! i)
      Confluence (i1, i2) o -> leastUpperBound (prev !! i1) (prev !! i2)
      Sink i                -> prev !! i

type BigT a = [a] -> [a]

eqsToBigT :: Lat a => [Equation a] -> BigT a
eqsToBigT eqs l = map ($ l) eqs

-- solve the recursive equations with the fixpoint theorem!
solveFix :: Lat a => [a] -> BigT a -> [a]
solveFix l bigT =
  let l' = bigT l
  in  if (l == l') then l else solveFix l' bigT

-- fixpoint operator!
fix :: (a -> a) -> a
fix f =
  let x = f x
  in  x

-- solveFix without explicit recursion
solveFix' :: Lat a => [a] -> BigT a -> [a]
solveFix' = fix (\f l bigT ->
                    let l' = bigT l
                    in if (l == l') then l else f l' bigT
                )


analyzeProg :: Lat a => Analysis a -> [Stmt] -> [(ID, a)]
analyzeProg anal prog = zip (map (fst . node) progps) $ solveFix initial bigT where
  initialL = (initialEnv anal) prog
  progps = cfgToProgP $ progToCfg prog
  bigT = eqsToBigT . progPsToEqs anal $ progps
  initial = replicate (length progps) initialL

pprintAnalysis :: (Lat a, Pretty a) => Analysis a -> [Stmt] -> IO ()
pprintAnalysis anal = mapM_ (\(i, r) -> putStrLn $ (show i ++ ": " ++ ppr r)) .
                        analyzeProg anal

printAnalysis :: (Lat a, Show a) => Analysis a -> [Stmt] -> IO ()
printAnalysis anal = mapM_ (\(i, r) -> putStrLn $ (show i ++ ": " ++ show r)) .
                        analyzeProg anal


data Transform a = Transform
  { transExpr :: a -> Expr -> Expr
  , transStmt :: a -> Stmt -> Stmt
  }

transformCfg :: Lat a => Transform a -> CFG -> Map ID a -> CFG
transformCfg transformer (CFG nodes) envMap =
  let (_, newNodes) = runState (helper S.empty (getNode 0)) nodes
  in  CFG newNodes where
    helper explored (k, node)
      | k `S.member` explored = return ()
      | otherwise = do
          let node' = trans node
          modify (M.insert k node')
          let explored' = S.insert k explored
          mapM_ (helper explored' . getNode) $ getOutgoing node
    trans node =
      case node of
        Source o              -> Source o
        Single s i o          -> Single    ((transStmt transformer) (getEnv i) s) i o
        CondITE e i bt bf c   -> CondITE   ((transExpr transformer) (getEnv i) e) i bt bf c
        CondWhile e i bt bf c -> CondWhile ((transExpr transformer) (getEnv i) e) i bt bf c
        Confluence (i1, i2) o -> Confluence (i1, i2) o
        Sink i                -> Sink i
    getNode i = (i,unsafeLookup i nodes)
    getEnv i = unsafeLookup i envMap

deadCodeElim :: CFG -> CFG
deadCodeElim (CFG nodes) =
  let (_, newNodes) = runState (helper S.empty =<< getNode 0) nodes
  in  CFG newNodes where
    helper :: Set ID -> (ID, Node) -> State (Map ID Node) ()
    helper explored (k, node)
      | k `S.member` explored = return ()
      | otherwise =
        let explored' = S.insert k explored
        in  case node of
          CondITE (BLit True) i bt bf c  -> (prune bf `andKeep` bt) k explored' i c
          CondITE (BLit False) i bt bf c -> (prune bt `andKeep` bf) k explored' i c
          CondWhile (BLit True) i bt bf c -> do
            -- remove false branch and never stop (until Sink of course).
            -- We have an infinite loop!
            removeBranch explored' k 0 =<< (getNode bf)
            helper explored' =<< (getNode bt)
          CondWhile (BLit False) i bt bf c -> do
             removeBranch explored' k c =<< (getNode bt)
             -- removeBranch has now set this conditional node's input to be
             -- the previous statement. So now, we can simply remove this
             -- node and wire its input into the false branch.
             (_, CondWhile _ i' _ _ _) <- getNode k
             modify (M.delete k)
             modify (M.adjust (setIn  i' k) bf) -- set false branch's input to i'
             modify (M.adjust (setOut bf k) i')
             helper explored' =<< (getNode bf)
          _ -> mapM_ (\n -> helper explored' =<< getNode n) $ getOutgoing node

    andKeep fn bt = fn bt
    prune bf = pruneAndKeep bf
    pruneAndKeep prune keep k explored' i c = do
      removeBranch explored' k c =<< (getNode prune)
      modify (M.delete k)
      modify (M.adjust (setOut keep k) i)
      modify (M.adjust (setIn  i  k) keep)
      helper explored' =<< (getNode keep)

    -- i is incoming node id
    removeBranch explored i c (k,Confluence (l,r) o)
      | k == c = do
          let newEnd = if l == i then r else l
          -- set the out-edge of newInd to o
          modify (M.adjust (setOut o k) newEnd)
          modify (M.adjust (setIn newEnd k) o)
          traceM $ "Deleting confluence " ++ show k
          modify (M.delete k)

    removeBranch explored i c (k,nd)
      | k `S.member` explored = return ()
      | otherwise = do
          let explored' = S.insert k explored
          modify (M.delete k)
          traceM $ "Deleting " ++ show k
          mapM_ (\n -> removeBranch explored' k c =<< getNode n) $ getOutgoing nd

    setIn  newi oldi nd = case nd of
      Source o              -> error "cant set incoming of Source"
      Single s i o          -> Single s newi o
      CondITE e i bt bf c   -> CondITE e newi bt bf c
      CondWhile e i bt bf c -> CondWhile e newi bt bf c
      Confluence (i1, i2) o ->
        let newins = (chngIfOldI i1, chngIfOldI i2)
            chngIfOldI i = if i == oldi then newi else i
        in  Confluence newins o
      Sink i                -> Sink newi

    setOut newo oldo nd = case nd of
        Source o              -> Source newo
        Single s i o          -> Single s i newo
        CondITE e i bt bf c   ->
          let (bt',bf') = (chngIfOldOut bt, chngIfOldOut bf)
          in CondITE e i bt' bf' c
        CondWhile e i bt bf c ->
          let (bt',bf') = (chngIfOldOut bt, chngIfOldOut bf)
          in CondWhile e i bt' bf' c
        Confluence (i1, i2) o -> Confluence (i1, i2) newo
        Sink i                -> error "cant set out of Sink"
      where
        chngIfOldOut o = if o == oldo then newo else o

    getNode :: ID -> State (Map ID Node) (ID, Node)
    getNode i = do
      n <- unsafeLookup i <$> get
      return (i, n)