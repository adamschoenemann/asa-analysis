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
program = spaces *> many (stmt <* spaces)

stmt :: Parser Stmt
stmt =  (const Skip) <$> trystring "skip" <* spaces <* char ';' <* spaces
    <|> ITE <$> (trystring "if" *> spaces1 *> expr) <*>
                     (trystring "then" *> spaces1 *> stmt <* spaces) <*>
                     (trystring "else" *> spaces1 *> stmt <* spaces)
    <|> While <$> (trystring "while" *> spaces1 *> expr) <*>
                     (trystring "do" *> spaces1 *> stmt) <* spaces
    <|> Output <$> (trystring "output" *> spaces1 *> expr <* char ';') <* spaces
    <|> Ass <$> (ident <* spaces) <*> (string ":=" *> spaces *> expr <* char ';') <* spaces
    <|> Block <$> (brackets block) <* spaces
      where
        block = sepBy stmt spaces

trystring :: String -> Parser String
trystring = try . string

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

unsafeParse :: String -> [Stmt]
unsafeParse p = either (error . show) id $ parse program "unsafe" p

parseCmm :: String -> Either ParseError [Stmt]
parseCmm p = parse program "cmm" p