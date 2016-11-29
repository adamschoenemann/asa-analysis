
module Text.Pretty where

class Pretty a where
  ppr :: a -> String
  putPrettyLn :: a -> IO ()
  putPrettyLn = putStrLn . ppr
