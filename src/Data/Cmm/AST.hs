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
  | ITE Expr Stmt Stmt
  | Block [Stmt]
  | While Expr Stmt
  | Output Expr
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData Stmt where

ppStmt :: Int -> Stmt -> String
ppStmt n stmt =
  let indent i = replicate (i*2) ' '
  in case stmt of
    Skip -> indent n ++ "skip;"
    Ass v e -> indent n ++ v ++ " := " ++ ppExpr e ++ ";"
    ITE e s1 s2 ->    indent n ++ "if " ++ ppExpr e ++ " then" ++ wsBlock s1
                   ++ (trimBlock s1 $ ppStmt (incBlock s1 n) s1) ++ "\n" ++ indent n ++ "else" ++ wsBlock s2
                   ++ (trimBlock s2 $ ppStmt (incBlock s2 n) s2)
    Block stmts -> indent n ++ "{\n" ++ unlines (map (ppStmt (n+1)) stmts) ++ indent n ++ "}"
    While e s  ->    indent n ++ "while " ++ ppExpr e ++ " do" ++ wsBlock s
                  ++ (trimBlock s $ ppStmt (incBlock s n) s)
    Output e   -> indent n ++ "output " ++ ppExpr e ++ ";"
  where
    -- whitespace composition
    wsBlock (Block _) = " "
    wsBlock _          = "\n"
    incBlock (Block _) j = j
    incBlock _         j = j + 1
    trimBlock (Block _) = trimHead
    trimBlock _         = id

instance Pretty Stmt where
  ppr = ppStmt 0

ppStmts :: Int -> [Stmt] -> String
ppStmts n = unlines . map (ppStmt n)

instance Pretty [Stmt] where
  ppr = ppStmts 0

stmtsToStmt :: [Stmt] -> Stmt
stmtsToStmt [x] = x
stmtsToStmt xs  = Block xs