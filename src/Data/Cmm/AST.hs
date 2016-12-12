{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, DeriveGeneric #-}
module Data.Cmm.AST where

import Utils

import Text.Pretty
import Data.Typeable
import Data.Data
import Control.DeepSeq
import GHC.Generics (Generic)

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Gt  Expr Expr
  | Lt  Expr Expr
  | Eq  Expr Expr
  | BLit Bool
  | ILit Int
  | Var String
  | Input
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData Expr where

ppExpr :: Expr -> String
ppExpr expression = help 0 expression where
  help :: Int -> Expr -> String
  help i expr =
    let parens p = if i > 0 then "(" ++ p ++ ")" else p
    in case expr of
      Add e1 e2 -> parens $ help (i+1) e1 ++ " + "  ++ help (i+1) e2
      Sub e1 e2 -> parens $ help (i+1) e1 ++ " - "  ++ help (i+1) e2
      Mul e1 e2 -> parens $ help (i+1) e1 ++ " * "  ++ help (i+1) e2
      Gt  e1 e2 -> parens $ help (i+1) e1 ++ " > "  ++ help (i+1) e2
      Lt  e1 e2 -> parens $ help (i+1) e1 ++ " < "  ++ help (i+1) e2
      Eq  e1 e2 -> parens $ help (i+1) e1 ++ " == " ++ help (i+1) e2
      ILit x  -> show x
      BLit b  -> uncapitalize (show b)
      Var n     -> n
      Input     -> "input"

instance Pretty Expr where
  ppr = ppExpr

data Stmt
  = Skip
  | Ass String Expr
  | Output Expr
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance Pretty Stmt where
  ppr stmt =
    case stmt of
      Skip       -> "skip;"
      (Ass v e)  -> v ++ " := " ++ ppExpr e ++ ";"
      (Output e) -> "output " ++ ppExpr e ++ ";"

data SubProg
  = ITE Expr SubProg SubProg
  | Block [SubProg]
  | While Expr SubProg
  | Single Stmt
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData SubProg where

ppSubProg :: Int -> SubProg -> String
ppSubProg n stmt =
  let indent i = replicate (i*2) ' '
  in case stmt of
    Single s    -> indent n ++ ppr s
    ITE e s1 s2 ->    indent n ++ "if " ++ ppExpr e ++ " then" ++ wsBlock s1
                   ++ (trimBlock s1 $ ppSubProg (incBlock s1 n) s1) ++ "\n" ++ indent n ++ "else" ++ wsBlock s2
                   ++ (trimBlock s2 $ ppSubProg (incBlock s2 n) s2)
    Block stmts -> indent n ++ "{\n" ++ unlines (map (ppSubProg (n+1)) stmts) ++ indent n ++ "}"
    While e s  ->    indent n ++ "while " ++ ppExpr e ++ " do" ++ wsBlock s
                  ++ (trimBlock s $ ppSubProg (incBlock s n) s)
  where
    -- whitespace composition
    wsBlock (Block _) = " "
    wsBlock _          = "\n"
    incBlock (Block _) j = j
    incBlock _         j = j + 1
    trimBlock (Block _) = trimHead
    trimBlock _         = id

instance Pretty SubProg where
  ppr = ppSubProg 0

ppSubProgs :: Int -> [SubProg] -> String
ppSubProgs n = unlines . map (ppSubProg n)

instance Pretty [SubProg] where
  ppr = ppSubProgs 0

stmtsToSubProg :: [SubProg] -> SubProg
stmtsToSubProg [x] = x
stmtsToSubProg xs  = Block xs