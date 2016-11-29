{-
  Module for parsing C--
  TBH, we should use a Lexer first, instead of lexing and parsing in one step
  but it works for now
-}
module Data.Cmm.Parser where

import Text.ParserCombinators.Parsec
import Data.Cmm.AST
import Data.Functor ((<$>))
import Control.Applicative ((<*>),(<*),(*>))
import Data.Char (toUpper)
import Utils

-- infixr 7 <**>

-- (<**>) :: Parser String -> Parser String -> Parser String
-- (<**>) x y = (x <* spaces) <*> (spaces *> y)

program :: Parser [Stmt]
program = many (stmt <* spaces)

stmt :: Parser Stmt
stmt =  (const Skip) <$> string "skip" <* spaces <* char ';' <* spaces
    <|> ITE <$> (string "if" *> spaces1 *> expr) <*>
                     (string "then" *> spaces1 *> stmt <* spaces) <*>
                     (string "else" *> spaces1 *> stmt <* spaces)
    <|> While <$> (string "while" *> spaces1 *> expr) <*>
                     (string "do" *> spaces1 *> stmt) <* spaces
    <|> Ass <$> (ident <* spaces) <*> (string ":=" *> spaces *> expr <* char ';') <* spaces
    <|> (brackets comp) <* spaces
      where
        comp = stmt `chainl1` (const Comp <$> spaces)

spaces1 = many1 space

expr :: Parser Expr
expr = equality
  where
    equality   = comparison `chainl1` cmpop
    comparison = intexpr    `chainl1` intop
    intexpr    = term       `chainl1` termop
    term       = factor     `chainl1` factop
    factor     = (parens expr) <* spaces <|> int <|> bool <|> var <|> input
    cmpop  =  const Eq  <$> string "==" <* spaces
    intop  =  const Gt  <$> char   '>'  <* spaces
          <|> const Lt  <$> char   '<'  <* spaces
    termop =  const Add <$> char   '+'  <* spaces
          <|> const Sub <$> char   '-'  <* spaces
    factop =  const Mul <$> char   '*'  <* spaces

var  = Var <$> ident <* spaces
int  = IConst . read <$> many1 digit <* spaces
bool = BConst . read . capitalize <$> (string "true" <|> string "false") <* spaces
input = const Input <$> string "input" <* spaces

parens x = between (char '(' <* spaces) (char ')') (x <* spaces)

brackets x = between (char '{' <* spaces) (char '}') (x <* spaces)

ident :: Parser String
ident  = (:) <$> letter <*> many alphaNum

