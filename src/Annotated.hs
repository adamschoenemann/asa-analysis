{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
-- annotated AST with lattice information
module Annotated where

import Data.CFG
import Data.Cmm.AST
import Data.Lat
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Utils


data Annotated a = AStmt a Stmt
                 | AITE a Expr (Annotated a) (Annotated a)
                 | AWhile a Expr (Annotated a)
                 | ABlock [Annotated a]


annotatedToProg :: [Annotated a] -> [Stmt]
annotatedToProg anns = map help anns where
  help ann =
    case ann of
      AStmt _ s -> s
      AITE _ e t f ->
        let tb = help t
            fb = help f
        in ITE e tb fb
      AWhile _ e t ->
        let tb = help t
        in While e tb
      ABlock xs -> Block $ foldr (:) [] $ annotatedToProg xs

cfgToAnnotated :: forall a. Lat a => Map ID a -> CFG -> [Annotated a]
cfgToAnnotated envMap cfg = dfFoldCFGAlg alg cfg [] where
  alg :: DFAlg [Annotated a]
  alg = DFAlg { dfWhile = while, dfITE = ite, dfSingle = single }
  single i s n =
    let env = getEnv i
    in  AStmt env s : n
  while i e t f =
    let env = getEnv i
        trb = annsToAnn t
    in  (AWhile env e trb) : f
  ite i e t f more =
    let env = getEnv i
        trb = annsToAnn t
        flb = annsToAnn f
    in AITE env e trb flb : more

  getEnv eid = unsafeLookup eid envMap

  annsToAnn [x] = x
  annsToAnn xs  = ABlock xs

