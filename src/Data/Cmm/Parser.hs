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

program :: Parser Program
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
    factor     = (parens expr) <* spaces <|> (try input) <|> int <|> bool <|> var
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

unsafeParse :: String -> Program
unsafeParse p = either (error . show) id $ parse program "unsafe" p

parseCmm :: String -> Either ParseError Program
parseCmm p = parse program "cmm" p