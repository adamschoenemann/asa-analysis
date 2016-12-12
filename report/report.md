# Automatic Software Analysis - A General Framework
By Oscar Toro and Adam Schønemann

## Introduction
This report will elaborate on the design and implementation of a general framework
for defining automatic software analyses on a small toy-programming language (C--).
The framework is implemented in Haskell.

## Overview
Software analysis is the process of taking a program as input, analysing that program
and give an approximation of a property of that program. This approximation can in
turn be used to optimize the program, guide the programmer, or warn the programmer
about potential errors.

### The language
In order to to analyze a program, one needs a concrete representation of that program
as data. The program we'll be analysing in this report, is the simple program called
C--.
The syntax for C-- is defined by the following grammar:
![c--](./imgs/c--.jpg)

In Haskell, we can use Algebraic Datatypes to model this grammar:
```Haskell
-- Expression grammar
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
  
-- Grammar for "simple" statements
data Stmt
  = Skip
  | Ass String Expr
  | Output Expr
  
-- Grammar for compound statements (or sub-programs)
data SubProg
  = ITE Expr SubProg SubProg
  | Block [SubProg]
  | While Expr SubProg
  | Single Stmt
```
Note some discrepancies between the grammar and the Haskell data-types:

- The expression grammar contains some extra operators as well as the Boolean
  literals
- The grammar for statements has been split in two:
    - Simple statements (`Output`, `Assign` and `Skip`)
    - Compund statements (`ITE`, `While`, `Block`)
        - These are statements that contain other statements, or "sub-programs"
- The variable declaration syntax (`var v;`) has been omitted, for simplicity

The reason for splitting the Statement grammar in two, is to have better type-safety
guarantees when implementing some algorithms later. Semantically, it is also
beneficial to distinguish between simple and compound statements.

### Parsing
In order to work with programs in C--, the programmer will write programs in textual
form, and a parser will take care of converting this form to the Haskell
representation shown above. An excerpt of the parser is shown below.

```Haskell
program :: Parser [SubProg]
program = spaces *> many (stmt <* spaces)

stmt :: Parser SubProg
stmt =  (const $ Single Skip) <$> trystring "skip" <* spaces <* char ';' <* spaces
    <|> ITE <$> (trystring "if" *> spaces1 *> expr) <*>
                     (trystring "then" *> spaces1 *> stmt <* spaces) <*>
                     (trystring "else" *> spaces1 *> stmt <* spaces)
    <|> While <$> (trystring "while" *> spaces1 *> expr) <*>
                     (trystring "do" *> spaces1 *> stmt) <* spaces
    <|> (Single . Output) <$> (trystring "output" *> spaces1 *> expr <* char ';') <* spaces
    <|> (\v e -> Single $ Ass v e) <$> (ident <* spaces) <*> (string ":=" *> spaces *> expr <* char ';') <* spaces
    <|> Block <$> (brackets block) <* spaces
      where
        block = sepBy stmt spaces
```

The parser is implemented using the `Parsec` library. Since we do not use a lexer,
we have to be careful with the spaces - but otherwise, the parser very closely
resembles the grammar. Note that the parser for a program returns a list of
subprograms. This notion of a program as a list of subprograms is used in the rest
of the implementation.