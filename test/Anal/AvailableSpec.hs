module Anal.AvailableSpec where

import Test.Hspec
import Data.Cmm.Parser
import TestPrograms
import Data.Either (isRight)
import Anal.Available
import Anal (analyzeProg, printAnalysis)
import Data.Cmm.AST
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Utils (unsafeLookup)

exp1 :: [(Int, [Expr])]
exp1 = [
     (0, [Mul (Var "a") (Var "b")])
    ,(1, [Mul (ILit 20) (Var "c"),   Mul (Var "a") (Var "b"),   Gt (Mul (Var "a") (Var "b")) (Mul (ILit 20) (Var "c"))])
    ,(2, [Add (ILit 20) (Var "a"),   Mul (ILit 20) (Var "c"),   Mul (Var "a") (Var "b"),   Gt (Mul (Var "a") (Var "b")) (Mul (ILit 20) (Var "c"))])
    ,(3, [Add (ILit 30) (Var "c"),   Mul (ILit 20) (Var "c"),   Mul (Var "a") (Var "b"),   Gt (Mul (Var "a") (Var "b")) (Mul (ILit 20) (Var "c"))])
    ,(4, [Mul (ILit 20) (Var "c"),   Mul (Var "a") (Var "b"),   Gt (Mul (Var "a") (Var "b")) (Mul (ILit 20) (Var "c"))])
    ,(5, [Mul (ILit 20) (ILit 30),   Mul (ILit 20) (Var "c"),   Mul (Var "a") (Var "b"),   Gt (Mul (Var "a") (Var "b")) (Mul (ILit 20) (Var "c"))])
    ,(6, [Mul (ILit 20) (ILit 30),   Mul (ILit 20) (Var "c")])
    ,(7, [Mul (ILit 20) (ILit 30),   Mul (ILit 20) (Var "c"),   Mul (Var "a") (Var "b")])
  ]

exp2 :: [(Int, [Expr])]
exp2 = [
     (0, [Mul (Var "a") (Var "b")])
    ,(1, [])
    ,(2, [Mul (ILit 20) (Var "c"),   Mul (Var "a") (Var "b"),   Gt (Mul (ILit 20) (Var "c")) (Mul (Var "a") (Var "b"))])
    ,(3, [Add (ILit 20) (Var "a"),   Mul (ILit 20) (Var "c")])
    ,(4, [Add (ILit 20) (Var "a"),   Sub (Var "c") (ILit 1)])
    ,(5, [Mul (ILit 20) (ILit 30),   Mul (ILit 20) (Var "c"),   Mul (Var "a") (Var "b"),   Gt (Mul (ILit 20) (Var "c")) (Mul (Var "a") (Var "b"))])
    ,(6, [Mul (ILit 20) (ILit 30),   Mul (ILit 20) (Var "c")])
    ,(7, [Mul (ILit 20) (ILit 30),   Mul (ILit 20) (Var "c"),   Mul (Var "a") (Var "b")])
  ]

exp3 :: [(Int, [Expr])]
exp3 = [
     (0, [Lt (ILit 2) (ILit 10)])
    ,(1, [Lt (ILit 2) (ILit 10)])
    ,(2, [Lt (ILit 2) (ILit 10)])
    ,(3, [Lt (ILit 2) (ILit 10)])
    ,(4, [Lt (ILit 2) (ILit 10)])
    ,(5, [Mul (ILit 42) (ILit 42),   Lt (ILit 2) (ILit 10)])
    ,(6, [Lt (ILit 2) (ILit 10)])
    ,(7, [Sub (ILit 100) (Mul (ILit 10) (ILit 10)),   Mul (ILit 10) (ILit 10),   Lt (ILit 2) (ILit 10)])
    ,(8, [Lt (ILit 2) (ILit 10)])
  ]

exp4 :: [(Int, [Expr])]
exp4 = [
     (0, [Lt (ILit 2) (ILit 10)])
    ,(1, [Lt (ILit 2) (ILit 10)])
    ,(2, [Lt (ILit 2) (ILit 10)])
    ,(3, [Lt (ILit 2) (ILit 10)])
    ,(4, [Add (ILit 10) (ILit 2),   Sub (Add (ILit 10) (ILit 2)) (ILit 1),   Mul (Var "y") (Sub (Add (ILit 10) (ILit 2)) (ILit 1)),   Lt (ILit 2) (ILit 10)])
    ,(5, [Lt (ILit 2) (ILit 10)])
    ,(6, [Sub (ILit 100) (Mul (ILit 10) (ILit 10)),   Mul (ILit 10) (ILit 10),   Lt (ILit 2) (ILit 10)])
    ,(7, [Lt (ILit 2) (ILit 10)])
  ]

exp5 :: [(Int, [Expr])]
exp5 = [
     (0, [])
    ,(1, [])
    ,(2, [])
    ,(3, [Mul (ILit 2) (ILit 20)])
    ,(4, [Mul (ILit 2) (ILit 20)])
    ,(5, [Mul (ILit 2) (ILit 20)])
    ,(6, [Mul (ILit 2) (ILit 20)])
    ,(7, [Mul (ILit 2) (ILit 20)])
  ]

expected :: Map String [(Int, [Expr])]
expected = M.fromList [("in1", exp1), ("in2", exp2), ("in3", exp3), ("in4", exp4), ("in5", exp5)]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Anal.Available" $ do
    it "should work" $ do
      mapM_ testAvailable testPrograms

testAvailable :: (String, String) -> IO ()
testAvailable (nm, progstr) = do
  let ep = parse program ("program" ++ nm) progstr
  ep `shouldSatisfy` isRight
  let Right p = ep
  let result = analyzeProg available p
  putStrLn $ nm ++ ":"
  printAnalysis available p
  (map tupSetToList result) `shouldBe` unsafeLookup nm expected
  putStrLn ""

tupSetToList :: (a, Set b) -> (a, [b])
tupSetToList (k,s) = (k, S.toList s)