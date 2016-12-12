-- a backwards analysis (with may)
module Anal.Liveness where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Cmm.AST
import Anal
import Text.Pretty
import Data.CFG

newtype LiveEnv = LEnv { unLive :: (Set String) } deriving (Ord, Eq, Show)

instance Lat LiveEnv where
  leastUpperBound (LEnv a) (LEnv b) = LEnv $ S.union a b
  bottom = const (LEnv S.empty)
  top    = LEnv . collectVars

instance Pretty LiveEnv where
  ppr = show

liveSingleToTFun :: Stmt -> TFun LiveEnv
liveSingleToTFun stmt = case stmt of
    Skip     -> id
    Ass v e  -> union (vars e) . delete v
    Output e -> union (vars e)
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


livenessAnal :: Analysis LiveEnv
livenessAnal =
  Analysis { singleToTFun = liveSingleToTFun
           , condToTFun = liveExprToTFun
           , getDeps    = backwards (LEnv S.empty)
           }
