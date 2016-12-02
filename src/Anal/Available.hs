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

availStmtToTFun :: Stmt -> TFun (Set Expr)
availStmtToTFun stmt = case stmt of
  Skip -> id
  Ass v e -> assign v e
  ITE e _ _ -> avail e
  Block _ -> id
  While e _ -> avail e
  Output e  -> avail e

collectExprs :: [Stmt] -> Set Expr
collectExprs stmts = foldl union S.empty $ map collectExprs' stmts where
  collectExprs' stmt = case stmt of
    Skip -> S.empty
    Ass _ e -> exprs e
    ITE e t f -> exprs e `union` collectExprs' t `union` collectExprs' f
    Block stmts' -> collectExprs stmts'
    While e s -> exprs e `union` collectExprs' s
    Output e  -> exprs e

instance Lat (Set Expr) where
  bottom = S.empty -- actually, this is top
  leastUpperBound = foldl1 S.intersection

available :: Analysis (Set Expr)
available =
  Analysis { stmtToTFun = availStmtToTFun -- :: Stmt -> TFun
           , exprToTFun = avail -- :: Expr -> TFun
           , initialEnv = collectExprs -- :: [Stmt] -> Set Expr
           , firstPPEnv = S.empty
           }