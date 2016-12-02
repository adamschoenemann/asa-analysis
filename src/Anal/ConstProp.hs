{-# LANGUAGE NamedFieldPuns, FlexibleInstances #-}

module Anal.ConstProp where

import Data.Set (Set, union, (\\))
import qualified Data.Set as S
import Anal
import Data.Cmm.AST
import Text.Pretty
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data CPLat
  = CPTop
  | CPBot
  | CPInt  Int
  | CPBool Bool
  deriving (Show, Eq, Ord)

instance Lat CPLat where
  bottom = CPBot

instance Pretty CPLat where
  ppr = show

type Env = Map String CPLat

instance Lat Env where
  bottom = M.empty

instance Pretty Env where
  ppr = show

getVar :: String -> Env -> CPLat
getVar n env = maybe CPTop id $ M.lookup n env

cpStmtToTFun :: Stmt -> TFun Env
cpStmtToTFun stmt = case stmt of
  Skip         -> id
  Ass v e      -> \env -> M.insert v (evalExpr e env) env
  ITE b s1 s2  -> id
  Block (s:ss) -> cpStmtToTFun s
  While b s    -> id
  Output e     -> id

evalExpr :: Expr -> Env -> CPLat
evalExpr e env = case e of
        Add e1 e2 -> evalExpr e1 env `add` evalExpr e2 env
        Sub e1 e2 -> evalExpr e1 env `sub` evalExpr e2 env
        Mul e1 e2 -> evalExpr e1 env `mul` evalExpr e2 env
        Gt  e1 e2 -> evalExpr e1 env `gt`  evalExpr e2 env
        Lt  e1 e2 -> evalExpr e1 env `lt`  evalExpr e2 env
        Eq  e1 e2 -> evalExpr e1 env `eq`  evalExpr e2 env
        BLit b    -> CPBool b
        ILit i    -> CPInt  i
        Var n     -> getVar n env
        Input     -> CPTop
  where
    latOp :: (Either (Int,Int) (Bool, Bool) -> CPLat) -> CPLat -> CPLat -> CPLat
    latOp _ CPTop _ = CPTop
    latOp _ _ CPTop = CPTop
    latOp _ CPBot _ = CPBot
    latOp _ _ CPBot = CPBot
    latOp op (CPInt i1)  (CPInt  i2) = op $ Left  (i1, i2)
    latOp op (CPBool b1) (CPBool b2) = op $ Right (b1, b2)
    latOp _ _ _ = invalid
    intop  :: ((Int, Int)   -> CPLat) -> CPLat -> CPLat -> CPLat
    boolop :: ((Bool, Bool) -> CPLat) -> CPLat -> CPLat -> CPLat
    intop  iop = latOp $ either iop (const invalid)
    boolop bop = latOp $ either (const invalid) bop
    add  = intop $ CPInt  . uncurry (+)
    sub  = intop $ CPInt  . uncurry (-)
    mul  = intop $ CPInt  . uncurry (*)
    gt   = intop $ CPBool . uncurry (>)
    lt   = intop $ CPBool . uncurry (<)
    eq   = latOp $ CPBool . either (uncurry (==)) (uncurry (==))
    invalid = error "invalid expression encountered"

cpExprToTFun :: Expr -> TFun Env
cpExprToTFun = const id

cpInitial :: [Stmt] -> Env
cpInitial = const bottom

envLUP :: [Env] -> Env
envLUP = intersectionsWith cpLUP where
  intersectionsWith fn = foldl1 (M.intersectionWith fn)
  cpLUP CPTop _  = CPTop
  cpLUP _ CPTop  = CPTop
  cpLUP CPBot l2 = l2
  cpLUP l1 CPBot = l1
  cpLUP l1 l2    =
    if l1 == l2 then l1 else CPTop

constProp :: Analysis Env
constProp =
  Analysis { stmtToTFun = cpStmtToTFun -- :: Stmt -> TFun
           , exprToTFun = cpExprToTFun -- :: Expr -> TFun
           , leastUpperBound = envLUP -- :: [Set Expr] -> Set Expr
           , initialLattice = cpInitial -- :: [Stmt] -> Set Expr
           }
