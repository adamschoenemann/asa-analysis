module Anal.DeadCode where

import Data.Set (Set)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Cmm.AST
import Data.CFG
import Control.Monad.State (runState, get, modify, State)
import Data.Functor ((<$>))
import Utils
import Anal
import Data.Cmm.Annotated

deadCodeTransAnn :: [Annotated a] -> [Stmt]
deadCodeTransAnn = deadCodeTrans . annotatedToProg

deadCodeTrans :: [Stmt] -> [Stmt]
deadCodeTrans ss = foldr dct [] ss where
  dct stmt acc =
    case stmt of
      Single _ -> stmt : acc
      ITE e bt bf ->
        case e of
          BLit True  -> dct bt acc
          BLit False -> dct bf acc
          _          -> ITE e (s2s $ dct bt []) (s2s $ dct bf []) : acc
      Block ss'   -> deadCodeTrans ss' ++ acc
      While e s   ->
        case e of
          BLit True  -> While e s : acc
          BLit False -> acc
          _          -> While e (s2s $ dct s []) : acc
  s2s = stmtsToStmt

deadCodeOpt :: Optimization
deadCodeOpt =
  Opt { optTransform  = deadCodeTransAnn :: [Annotated UnitLat] -> [Stmt]
      , optAnalysis   = idAnalysis
      }

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
          NITE (BLit True) i bt bf c  -> (prune bf `andKeep` bt) k explored' i c
          NITE (BLit False) i bt bf c -> (prune bt `andKeep` bf) k explored' i c
          NWhile (BLit True) i bt bf c -> do
            -- remove false branch and never stop (until NSink of course).
            -- We have an infinite loop!
            removeBranch explored' k 0 =<< (getNode bf)
            -- modify (M.delete k) -- delete the while condition
            -- modify (M.adjust (setOut bt k) i) -- point the previous to the true branch
            -- modify (M.adjust (setIn i k)  bt) -- point the true branch's in to previous
            helper explored' =<< (getNode bt)
            modify (M.insert bf (NSink i))
          NWhile (BLit False) i bt bf c -> do
             removeBranch explored' k c =<< (getNode bt)
             -- removeBranch has now set this conditional node's input to be
             -- the previous statement. So now, we can simply remove this
             -- node and wire its input into the false branch.
             (_, NWhile _ i' _ _ _) <- getNode k
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
    removeBranch explored i c (k,NConfl (l,r) o)
      | k == c = do
          let newEnd = if l == i then r else l
          -- set the out-edge of newInd to o
          modify (M.adjust (setOut o k) newEnd)
          modify (M.adjust (setIn newEnd k) o)
          -- traceM $ "Deleting confluence " ++ show k
          modify (M.delete k)

    removeBranch explored i c (k,nd)
      | k `S.member` explored = return ()
      | otherwise = do
          let explored' = S.insert k explored
          modify (M.delete k)
          -- traceM $ "Deleting " ++ show k
          mapM_ (\n -> removeBranch explored' k c =<< getNode n) $ getOutgoing nd

    setIn  newi oldi nd = case nd of
      NSource o              -> error "cant set incoming of NSource"
      NSingle s i o          -> NSingle s newi o
      NITE e i bt bf c   -> NITE e newi bt bf c
      NWhile e i bt bf c -> NWhile e newi bt bf c
      NConfl (i1, i2) o ->
        let newins = (chngIfOldI i1, chngIfOldI i2)
            chngIfOldI i = if i == oldi then newi else i
        in  NConfl newins o
      NSink i                -> NSink newi

    setOut newo oldo nd = case nd of
        NSource o              -> NSource newo
        NSingle s i o          -> NSingle s i newo
        NITE e i bt bf c   ->
          let (bt',bf') = (chngIfOldOut bt, chngIfOldOut bf)
          in NITE e i bt' bf' c
        NWhile e i bt bf c ->
          let (bt',bf') = (chngIfOldOut bt, chngIfOldOut bf)
          in NWhile e i bt' bf' c
        NConfl (i1, i2) o -> NConfl (i1, i2) newo
        NSink i                -> error "cant set out of NSink"
      where
        chngIfOldOut o = if o == oldo then newo else o

    getNode :: ID -> State (Map ID Node) (ID, Node)
    getNode i = do
      n <- unsafeLookup i <$> get
      return (i, n)