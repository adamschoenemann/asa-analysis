{-# LANGUAGE NamedFieldPuns, FlexibleInstances #-}

module Avail
  ( module Avail, module Anal ) where

-- import Control.Monad.Fix
-- import Data.Set (Set, union, (\\))
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as M
-- import qualified Data.Set as S
-- import Control.Monad.State
-- import Data.Functor ((<$>))
-- import Data.List (sort)
-- import Data.Graph
-- import Data.Cmm.AST
-- import Anal.CFG
-- import Text.Pretty
-- import Data.List (intercalate)
import Data.Cmm.AST
import Anal
import Utils
import Data.Map.Strict (Map)
import Data.CFG
import qualified Data.Map.Strict as M
import Control.Monad.State
import Debug.Trace


deadCodeElim :: CFG -> CFG
deadCodeElim (CFG nodes) =
  let (_, newNodes) = runState (helper $ getNode 0) nodes
  in  CFG newNodes where
    helper (k, node) =
      trace "helper" $
      case node of
        CondITE (BLit True) i bt bf c -> do
           removeBranch k c (getNode bf)
           modify (M.delete k)
           modify (M.adjust (setOut bt k) i)
        CondITE (BLit False) i bt bf c -> do
           removeBranch k c (getNode bt)
           modify (M.delete k)
           modify (M.adjust (setOut bf k) i)
        nd -> mapM_ (helper . getNode) $ getOutgoing nd
    -- i is incoming node id
    removeBranch i c (k,Confluence (l,r) o) | k == c = do
      let newEnd = if l == i then r else l
      -- set the out-edge of newInd to o
      modify (M.adjust (setOut o k) newEnd)
      modify (M.adjust (setIn newEnd k) o)
      traceM $ "Deleting confluence " ++ show k
      modify (M.delete k)

    removeBranch i c (k,nd) = do
      modify (M.delete k)
      traceM $ "Deleting " ++ show k
      mapM_ (removeBranch k c . getNode) $ getOutgoing nd

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

    getNode i = (i,unsafeLookup i nodes )