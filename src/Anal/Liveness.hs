-- a backwards analysis (with may)
module Anal.Liveness where

import Data.Set (Set)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Cmm.AST
import Data.CFG
import Data.Functor ((<$>))
import Utils
import Anal
import Data.Cmm.Annotated
import Text.Pretty


newtype LiveEnv = LEnv { unLive :: (Set String) } deriving (Ord, Eq, Show)

instance Lat LiveEnv where
  leastUpperBound (LEnv a) (LEnv b) = LEnv $ S.union a b
  bottom = LEnv S.empty

instance Pretty LiveEnv where
  ppr = show

liveStmtToTFun :: Stmt -> TFun LiveEnv
liveStmtToTFun stmt = case stmt of
    Skip         -> id
    Ass v e      -> union (vars e) . delete v
    ITE e _ _    -> error "ITE shouldn't happen"
    Block _      -> error "Block should not happen"
    While e _    -> error "While should not happen"
    Output e     -> union (vars e)
  where
    union e = LEnv . S.union (unLive e) . unLive
    delete v = LEnv . S.delete v . unLive


liveExprToTFun :: Expr -> TFun LiveEnv
liveExprToTFun e = LEnv . S.union (unLive . vars $ e) . unLive

vars :: Expr -> LiveEnv
vars = LEnv . help where
  help expr =
    case expr of
      Add e1 e2 -> help e1 `S.union` help e2
      Sub e1 e2 -> help e1 `S.union` help e2
      Mul e1 e2 -> help e1 `S.union` help e2
      Gt  e1 e2 -> help e1 `S.union` help e2
      Lt  e1 e2 -> help e1 `S.union` help e2
      Eq  e1 e2 -> help e1 `S.union` help e2
      BLit b    -> S.empty
      ILit i    -> S.empty
      Var n     -> S.singleton n
      Input     -> S.empty

liveInitial :: CFG -> LiveEnv
liveInitial = const (LEnv S.empty)

livenessAnal :: Analysis LiveEnv
livenessAnal =
  Analysis { stmtToTFun = liveStmtToTFun
           , condToTFun = liveExprToTFun
           , initialEnv = liveInitial
           , getDeps    = backwards (LEnv S.empty)
           }

-- livenessOpt :: Optimization
-- livenessOpt =
--   Opt { optTransform  = lTransform
--       , optAnalysis   = livenessAnal
--       }