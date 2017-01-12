{-
Module defining programs that are annotated with information
from an analysis
-}
{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
module Data.Cmm.Annotated where

import Data.CFG
import Data.Cmm.AST
import Data.Lat
import Data.Map.Strict (Map)
import Utils

-- Like Data.Cmm.AST but with an annotation
data Annotated a = ASingle a Stmt
                 | AITE a Expr (Annotated a) (Annotated a)
                 | AWhile a Expr (Annotated a)
                 | ABlock [Annotated a]

-- injection from Program to [Annotated ()]
injectProgram :: Program -> [Annotated ()]
injectProgram = map inject where
  inject (Single x)  = ASingle () x
  inject (ITE b t f) = AITE () b (inject t) (inject f)
  inject (While b t) = AWhile () b (inject t)
  inject (Block xs)  = ABlock (map inject xs)

-- annotated algebra for folding
-- a is the annotated's type param
-- b is the type of the accumulator
data AnnAlg a b = AnnAlg
  { aaSubProg  :: a -> Stmt -> b -> b
  , aaITE      :: a -> Expr -> b -> b -> b -> b
  , aaWhile    :: a -> Expr -> b -> b -> b
  , aaBlock    :: [b] -> b -> b
  }

-- interpret annotated AST into a "plain" AST
interpAnn :: (a -> SubProg -> SubProg) -> Annotated a -> SubProg
interpAnn fn ann = help ann where
  help ann = case ann of
    ASingle a s    -> fn a (Single s)
    AITE a e bt bf -> fn a (ITE e (help bt) (help bf))
    AWhile a e bt  -> fn a (While e (help bt))
    ABlock as      -> Block (map help as)

-- foldr :: (a -> b -> a) -> b -> [a] -> b
foldAnn :: AnnAlg a b -> b -> Annotated a -> b
foldAnn (AnnAlg stmt ite while block) seed annotated = help annotated seed where
  help ann acc =
    case ann of
      ASingle a s    -> stmt a s acc
      AITE a e bt bf -> ite a e (help bt seed) (help bf seed) acc
      AWhile a e bt  -> while a e (help bt seed) acc
      ABlock as      -> block (map (flip help $ seed) as) acc

-- inverse of injectProgram
annotatedToProg :: [Annotated a] -> Program
annotatedToProg = map help where
  help ann = s2s $ foldAnn alg [] ann
  alg = AnnAlg stmt ite while block
  stmt _ s acc      = Single s : acc
  ite _ e bt bf acc = ITE e (s2s bt) (s2s bf) : acc
  while _ e bt acc  = While e (s2s bt) : acc
  block bs acc      = s2s (concat bs) : acc
  s2s = stmtsToSubProg

-- Convert a CFG + the result of an analysis (solution to recursive equations)
-- to an annotated AST.
-- forall a. is only used for ScopedTypeVariables
cfgToAnnotated :: forall a. Lat a => Map ID a -> CFG -> [Annotated a]
cfgToAnnotated envMap cfg = dfFoldCFGAlg alg cfg [] where
  alg :: DFAlg [Annotated a] -- here, ScopedTypeVariables is used
  alg = DFAlg { dfWhile = while, dfITE = ite, dfNSingle = single }
  single i s n =
    let env = getEnv i
    in  ASingle env s : n
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

