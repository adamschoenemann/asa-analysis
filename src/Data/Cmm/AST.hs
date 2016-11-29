{-# LANGUAGE FlexibleInstances #-}
module Data.Cmm.AST where

import Utils

import Text.Pretty

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Gt  Expr Expr
  | Lt  Expr Expr
  | Eq  Expr Expr
  | BConst Bool
  | IConst Int
  | Var String
  | Input
  deriving (Eq, Ord, Show)

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
      IConst x  -> show x
      BConst b  -> uncapitalize (show b)
      Var n     -> n
      Input     -> "input"

instance Pretty Expr where
  ppr = ppExpr

data Stmt
  = Skip
  | Ass String Expr
  | ITE Expr Stmt Stmt
  | Comp Stmt Stmt
  | While Expr Stmt
  deriving (Eq, Ord, Show)

ppStmt :: Int -> Stmt -> String
ppStmt n stmt =
  let ident i = replicate (i*2) ' '
  in case stmt of
    Skip -> ident n ++ "skip;"
    Ass v e -> ident n ++ v ++ " := " ++ ppExpr e ++ ";"
    ITE e s1 s2 ->    ident n ++ "if " ++ ppExpr e ++ " then {\n"
                   ++ ppStmt (n+1) s1 ++ "\n" ++ ident n ++ "} else {\n"
                   ++ ppStmt (n+1) s2 ++ "\n" ++ ident n ++ "}"
    Comp s1 s2 -> ppStmt n s1 ++ "\n" ++ ppStmt n s2
    While e s  ->    ident n ++ "while " ++ ppExpr e ++ " do {\n"
                  ++ ppStmt (n+1) s ++ "\n" ++ ident n ++ "}"

instance Pretty Stmt where
  ppr = ppStmt 0

instance Pretty [Stmt] where
  ppr = unlines . map ppr

-- Composition to list of statements
-- comp2list :: Stmt -> [Stmt]
-- comp2list (Comp s1 c2@(Comp s2 s3)) = s1 : comp2list c2
-- comp2list (Comp c1@(Comp s1 s2) s3) = comp2list c1 ++ [s3]
-- comp2list (Comp s1 s2) = [s1,s2]
-- comp2list _ = error "only flatten compositions"