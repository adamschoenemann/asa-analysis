{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
-- annotated AST with lattice information
module Data.Cmm.Annotated where

import Data.CFG
import Data.Cmm.AST
import Data.Lat
import Data.Map.Strict (Map)
import Utils


data Annotated a = ASingle a Stmt
                 | AITE a Expr (Annotated a) (Annotated a)
                 | AWhile a Expr (Annotated a)
                 | ABlock [Annotated a]

-- annotated algebra for folding
-- a is the annotated's type param
-- b is the type of the accumulator
data AnnAlg a b = AnnAlg
  { aaSubProg  :: a -> Stmt -> b -> b
  , aaITE   :: a -> Expr -> b -> b -> b -> b
  , aaWhile :: a -> Expr -> b -> b -> b
  , aaBlock :: [b] -> b -> b
  }

-- foldr :: (a -> b -> a) -> b -> [a] -> b
mapAnn :: (a -> SubProg -> SubProg) -> Annotated a -> SubProg
mapAnn fn ann = help ann where
  help ann = case ann of
    ASingle a s    -> fn a (Single s)
    AITE a e bt bf -> fn a (ITE e (help bt) (help bf))
    AWhile a e bt  -> fn a (While e (help bt))
    ABlock as      -> Block (map help as)

foldAnn :: AnnAlg a b -> b -> Annotated a -> b
foldAnn (AnnAlg stmt ite while block) seed annotated = help annotated seed where
  help ann acc =
    case ann of
      ASingle a s      -> stmt a s acc
      AITE a e bt bf -> ite a e (help bt seed) (help bf seed) acc
      AWhile a e bt  -> while a e (help bt seed) acc
      ABlock as      -> block (map (flip help $ seed) as) acc

annotatedToProg :: [Annotated a] -> Program
annotatedToProg = map help where
  help ann = s2s $ foldAnn alg [] ann
  alg = AnnAlg stmt ite while block
  stmt _ s acc  = Single s : acc
  ite _ e bt bf acc = ITE e (s2s bt) (s2s bf) : acc
  while _ e bt acc  = While e (s2s bt) : acc
  block bs acc      = s2s (concat bs) : acc
  s2s = stmtsToSubProg

-- annotatedToProg' :: [Annotated a] -> Program
-- annotatedToProg' anns = map help anns where
--   help ann =
--     case ann of
--       ASingle _ s -> s
--       AITE _ e t f ->
--         let tb = help t
--             fb = help f
--         in ITE e tb fb
--       AWhile _ e t ->
--         let tb = help t
--         in While e tb
--       ABlock xs -> Block $ foldr (:) [] $ annotatedToProg' xs

cfgToAnnotated :: forall a. Lat a => Map ID a -> CFG -> [Annotated a]
cfgToAnnotated envMap cfg = dfFoldCFGAlg alg cfg [] where
  alg :: DFAlg [Annotated a]
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

