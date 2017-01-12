{-# LANGUAGE NamedFieldPuns, FlexibleInstances #-}

module Anal.ConstProp where

import Anal
import Data.Cmm.AST
import Data.CFG
import Text.Pretty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Cmm.Annotated
import Control.DeepSeq
import qualified Data.Set as S

data CPLat
  = CPTop
  | CPBot
  | CPInt  Int
  | CPBool Bool
  deriving (Show, Eq, Ord)

instance NFData CPLat where

instance Lat CPLat where
  bottom = const CPBot
  leastUpperBound = cpLUP
  top    = const CPTop

cpLUP :: CPLat -> CPLat -> CPLat
cpLUP CPTop _  = CPTop
cpLUP _ CPTop  = CPTop
cpLUP CPBot l2 = l2
cpLUP l1 CPBot = l1
cpLUP l1 l2    =
  if l1 == l2 then l1 else CPTop

instance Pretty CPLat where
  ppr = show

-- map from var names to CPLat (are they constant?)
type Env = Map String CPLat

instance Lat Env where
  bottom = M.fromList . S.toList . S.map (\v -> (v, CPBot)) . collectVars
  leastUpperBound = envLUP
  top = const M.empty

instance Pretty Env where
  ppr = show

getVar :: String -> Env -> CPLat
getVar n env = maybe CPBot id $ M.lookup n env

cpNodeToTFun :: Node -> TFun Env
cpNodeToTFun node = case node of
    NSingle stmt _ _  -> cpSingleToTFun stmt
    NITE   e _ _ _ _  -> id
    NWhile e _ _ _ _  -> id
    NSource _         -> id
    NConfl  _ _       -> id
    NSink   _         -> id
  where
    cpSingleToTFun stmt = case stmt of
      Skip     -> id
      Ass v e  -> \env -> M.insert v (evalExpr e env) env
      Output _ -> id

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
    -- an operator that takes two ints an input
    intop  :: ((Int, Int)   -> CPLat) -> CPLat -> CPLat -> CPLat
    intop  iop = latOp $ either iop (const invalid)
    -- an operator that takes two bools as input
    -- boolop :: ((Bool, Bool) -> CPLat) -> CPLat -> CPLat -> CPLat
    -- boolop bop = latOp $ either (const invalid) bop
    add  = intop  $ CPInt  . uncurry (+)
    sub  = intop  $ CPInt  . uncurry (-)
    mul  = intop  $ CPInt  . uncurry (*)
    gt   = intop $ CPBool . uncurry (>)
    lt   = intop $ CPBool . uncurry (<)
    eq   = latOp $ CPBool . either (uncurry (==)) (uncurry (==))
    invalid = error "invalid expression encountered"

envLUP :: Env -> Env -> Env
envLUP = M.intersectionWith cpLUP

constProp :: Analysis Env
constProp =
  Analysis { nodeToTFun = cpNodeToTFun
           , getDeps = forward (M.empty)
           }


cpLatIntCombine :: (Int -> Int -> Int) -> CPLat -> CPLat -> CPLat
cpLatIntCombine fn (CPInt a) (CPInt b) = CPInt (fn a b)
cpLatIntCombine _  (CPBool x) _        = error "type error"
cpLatIntCombine _  _ (CPBool x)        = error "type error"
cpLatIntCombine _  a b                 = cpLUP a b

cpLatIntToBoolCombine :: (Int -> Int -> Bool) -> CPLat -> CPLat -> CPLat
cpLatIntToBoolCombine fn (CPInt a) (CPInt b) = CPBool (fn a b)
cpLatIntToBoolCombine _  (CPBool x) _        = error "type error"
cpLatIntToBoolCombine _  _ (CPBool x)        = error "type error"
cpLatIntToBoolCombine _  a b                 = cpLUP a b

cpLatEqCombine :: CPLat -> CPLat -> CPLat
cpLatEqCombine (CPInt a) (CPInt b)    = CPBool (a == b)
cpLatEqCombine (CPBool b) (CPBool a)  = CPBool (a == b)
cpLatEqCombine (CPInt _) (CPBool _)   = error "type error"
cpLatEqCombine (CPBool _) (CPInt _)   = error "type error"
cpLatEqCombine a b                    = cpLUP a b


cpTransform :: [Annotated Env] -> Program
cpTransform anns = map (interpAnn stmt) anns where
  stmt :: Env -> SubProg -> SubProg
  stmt env st = case st of
    Single (Skip    ) -> Single Skip
    Single (Ass v e ) -> Single $ Ass v (expr env e)
    Single (Output e) -> Single $ Output (expr env e)
    ITE e bt bf       -> ITE (expr env e) bt bf
    Block ss          -> Block ss
    While e s         -> While (expr env e) s
  expr :: Env -> Expr -> Expr
  expr env e = maybe e id (cLatToMaybeExpr . isConst env $ e) where
    cLatToMaybeExpr cl = case cl of
      CPTop -> Nothing
      CPBot -> Nothing
      CPInt  i -> return $ ILit i
      CPBool b -> return $ BLit b
    isConst env e = case e of
      Add e1 e2 -> cpLatIntCombine       (+)  (isConst env e1) (isConst env e2)
      Sub e1 e2 -> cpLatIntCombine       (-)  (isConst env e1) (isConst env e2)
      Mul e1 e2 -> cpLatIntCombine       (*)  (isConst env e1) (isConst env e2)
      Gt  e1 e2 -> cpLatIntToBoolCombine (>)  (isConst env e1) (isConst env e2)
      Lt  e1 e2 -> cpLatIntToBoolCombine (<)  (isConst env e1) (isConst env e2)
      Eq  e1 e2 -> cpLatEqCombine (isConst env e1) (isConst env e2)
      ILit x  -> CPInt x
      BLit b  -> CPBool b
      Var n     -> maybe CPTop id $ M.lookup n env
      Input     -> CPTop

constPropOpt :: Optimization
constPropOpt =
  Opt { optTransform  = cpTransform
      , optAnalysis   = constProp
      }
