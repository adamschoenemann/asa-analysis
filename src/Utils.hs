
module Utils where

import Data.Char (toUpper, toLower, isSpace)
import Data.Functor ((<$>))

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x,xs)

capitalize :: String -> String
capitalize s = maybe "" id $ (\(x,xs) -> toUpper x : xs) <$> uncons s

uncapitalize :: String -> String
uncapitalize s = maybe "" id $ (\(x,xs) -> toLower x : xs) <$> uncons s

trimHead :: String -> String
trimHead = dropWhile isSpace