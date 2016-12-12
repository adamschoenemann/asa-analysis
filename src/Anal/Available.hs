{-# LANGUAGE NamedFieldPuns, FlexibleInstances #-}

module Anal.Available
 ( available ) where

import Data.Set (Set, union, (\\))
import qualified Data.Set as S
import Anal
import Data.Cmm.AST
import Text.Pretty
import Data.List (intercalate)

instance Pretty (Set Expr) where
  ppr = intercalate ", " . map ppr . S.toList

exprs :: Expr -> Set Expr
exprs expr = case expr of
  e@(Add e1 e2) -> S.singleton e `union` exprs e1 `union` exprs e2
  e@(Sub e1 e2) -> S.singleton e `union` exprs e1 `union` exprs e2
  e@(Mul e1 e2) -> S.singleton e `union` exprs e1 `union` exprs e2
  e@(Gt  e1 e2) -> S.singleton e `union` exprs e1 `union` exprs e2
  e@(Lt  e1 e2) -> S.singleton e `union` exprs e1 `union` exprs e2
  e@(Eq  e1 e2) -> S.singleton e `union` exprs e1 `union` exprs e2
  ILit _    -> S.empty
  BLit _    -> S.empty
  Var   _     -> S.empty
  Input       -> S.empty

avail :: Expr -> Set Expr -> Set Expr
avail e l    = l `union` exprs e

unavail :: String -> Set Expr -> Set Expr
unavail var l  = l \\ S.filter (occursIn var) l where
  occursIn v e = case e of
    Add e1 e2 -> occursIn v e1 || occursIn v e2
    Sub e1 e2 -> occursIn v e1 || occursIn v e2
    Mul e1 e2 -> occursIn v e1 || occursIn v e2
    Gt  e1 e2 -> occursIn v e1 || occursIn v e2
    Lt  e1 e2 -> occursIn v e1 || occursIn v e2
    Eq  e1 e2 -> occursIn v e1 || occursIn v e2
    ILit _  -> False
    BLit _  -> False
    Var s     -> s == v
    Input     -> False

assign :: String -> Expr -> Set Expr -> Set Expr
assign v e  = avail e . unavail v

availSingleToTFun :: Stmt -> TFun (Set Expr)
availSingleToTFun stmt = case stmt of
  Skip     -> id
  Ass v e  -> assign v e
  Output e -> avail e

instance Lat (Set Expr) where
  bottom = collectExprs
  leastUpperBound = S.intersection
  top    = const S.empty

available :: Analysis (Set Expr)
available =
  Analysis { singleToTFun = availSingleToTFun -- :: SubProg -> TFun
           , condToTFun = avail
           , getDeps = forward S.empty
           }

collectExprs :: Program -> Set Expr
collectExprs prog = foldr (\x acc -> help x `union` acc) (S.empty) prog where
  help subprog =
    case subprog of
      Single single ->
        case single of
          Skip      -> S.empty
          Ass _ e   -> exprs e
          Output e  -> exprs e
      ITE e bt bf   -> exprs e `union` help bt `union` help bf
      While e bt    -> exprs e `union` help bt
      Block subps   -> collectExprs subps