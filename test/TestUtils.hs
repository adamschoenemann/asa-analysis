
module TestUtils where

output :: String -> IO ()
output = const (return ()) -- putStrLn