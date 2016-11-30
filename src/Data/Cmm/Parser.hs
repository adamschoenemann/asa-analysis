{-
  Module for parsing C--
  TBH, we should use a Lexer first, instead of lexing and parsing in one step
  but it works for now
-}
module Data.Cmm.Parser
  ( module Data.Cmm.Parser
  , parse
  ) where

import Text.ParserCombinators.Parsec
import Data.Cmm.AST
import Data.Functor ((<$>))
import Control.Applicative ((<*>),(<*),(*>))
import Utils

program :: Parser [Stmt]
program = many (stmt <* spaces)

stmt :: Parser Stmt
stmt =  (const Skip) <$> string "skip" <* spaces <* char ';' <* spaces
    <|> ITE <$> (string "if" *> spaces1 *> expr) <*>
                     (string "then" *> spaces1 *> stmt <* spaces) <*>
                     (string "else" *> spaces1 *> stmt <* spaces)
    <|> While <$> (string "while" *> spaces1 *> expr) <*>
                     (string "do" *> spaces1 *> stmt) <* spaces
    <|> Output <$> (string "output" *> spaces1 *> expr <* char ';') <* spaces
    <|> Ass <$> (ident <* spaces) <*> (string ":=" *> spaces *> expr <* char ';') <* spaces
    <|> Block <$> (brackets block) <* spaces
      where
        block = sepBy stmt spaces

spaces1 :: Parser String
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

var, int, bool, input :: Parser Expr
var  = Var <$> ident <* spaces
int  = ILit . read <$> many1 digit <* spaces
bool = BLit . read . capitalize <$> (string "true" <|> string "false") <* spaces
input = const Input <$> string "input" <* spaces

parens, brackets :: Parser a -> Parser a
parens x = between (char '(' <* spaces) (char ')') (x <* spaces)
brackets x = between (char '{' <* spaces) (char '}') (x <* spaces)

ident :: Parser String
ident  = (:) <$> letter <*> many alphaNum

