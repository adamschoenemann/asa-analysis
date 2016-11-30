{-# LANGUAGE NamedFieldPuns, FlexibleInstances #-}

module Avail
  ( module Avail, module Anal ) where

import Control.Monad.Fix
import Data.Set (Set, union, (\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State
import Data.Functor ((<$>))
import Data.List (sort)
import Data.Graph
import Data.Cmm.AST
import Anal.CFG
import Text.Pretty
import Data.List (intercalate)
import Anal


prog1 :: [Stmt]
prog1 =
  [ Ass "x" (Var "a" `Mul` Var "b")
  , (ITE ((Var "a" `Mul` Var "b") `Gt` (ILit 20 `Mul` Var "c"))
       (Ass "y" (ILit 20 `Add` Var "a"))
       (Ass "y" (ILit 30 `Add` Var "c"))
  )
  , Ass "z" (ILit 20 `Mul` ILit 30)
  , Ass "a" (ILit 20)
  , Ass "u" (Var "a" `Mul` Var "b")
  ]

prog2 :: [Stmt]
prog2 =
  [ Ass "x" (Var "a" `Mul` Var "b")
  , While ((ILit 20 `Mul` Var "c") `Gt` (Var "a" `Mul` Var "b"))
       (Block [ (Ass "a" (ILit 20 `Add` Var "a"))
              , (Ass "c" (Var "c" `Sub` ILit 1))
              ])
  , Ass "z" (ILit 20 `Mul` ILit 30)
  , Ass "a" (ILit 20)
  , Ass "u" (Var "a" `Mul` Var "b")
  ]


cfg1 :: Graph CFGNodeData CFGEdgeData
cfg1 = Graph {
      nodes = nodes
    , edges = S.fromList $ map (\(u,v,d) -> Edge (u,v) d) edges
    , src = 0, sink = 7
  } where
      nodes = M.fromList $ zipWith (\i a -> (i, Node i a)) [0..]
                [ CFGStmt $ Ass "x" (Var "a" `Mul` Var "b")   -- x := a * b  (0)
                , Cond ((Var "a" `Mul` Var "b") `Gt` (ILit 20 `Mul` Var "c")) -- if (a * b > 20 * c) (1)
                , CFGStmt $ Ass "y" (ILit 20 `Add` Var "a")  -- y := 20 + a (2)
                , CFGStmt $ Ass "y" (ILit 30 `Add` Var "c")  -- y := 30 + c (3)
                , ConfPoint
                , CFGStmt $ Ass "z" (ILit 20 `Mul` ILit 30) -- z := 20 * 30 (4)
                , CFGStmt $ Ass "a" (ILit 20)                -- a := 20 (5)
                , CFGStmt $ Ass "u" (Var "a" `Mul` Var "b")   -- u := a * b (6)
                ]
      no = NoData
      edges = [(0,1,no), (1,2, Branch True), (1,3, Branch False), (3,4,no), (2,4,no), (4,5,no), (5,6,no), (6,7,no)]

cfg1' :: Graph CFGNodeData CFGEdgeData
cfg1' = cfg prog1

cfg2 :: Graph CFGNodeData CFGEdgeData
cfg2 = cfg prog2


