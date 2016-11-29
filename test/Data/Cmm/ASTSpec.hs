{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Data.Cmm.ASTSpec (main, spec) where

import Data.Cmm.AST()
import Data.Cmm.Parser
import Test.Hspec
import qualified NeatInterpolation as N
import Text.Pretty
import Text.ParserCombinators.Parsec
import Data.Either (isRight)

main :: IO ()
main = hspec spec

in1, in2, in3 :: String

in1 =
  [N.string|
  x := a * b;
  if (a * b) > (20 * c) then {
    y := 20 + a;
  } else {
    y := 30 + c;
  }
  z := 20 * 30;
  a := 20;
  u := a * b;|]

in2 =
  [N.string|
    x := a * b;
    while (20 * c) > (a * b) do {
      a := 20 + a;
      c := c - 1;
    }
    z := 20 * 30;
    a := 20;
    u := a * b;|]
in3 =
  [N.string|
    x := 2 < 10;
    if x then {
      if false then {
        y := true;
      } else {
        y := false;
      }
    } else {
      y := 100 - (10 * 10);
    }
  |]

inputs :: [String]
inputs = [in1, in2, in3]

spec :: Spec
spec = do
  describe "Data.Cmm.AST" $ do
    -- of course, this is not generally true, but for these
    -- specific programs, it should be
    it "satisfies id = ppr . parse" $ do
      mapM_ (\p -> do
                let ep = parse program "program" p
                ep `shouldSatisfy` isRight
                let Right p' = ep
                p `shouldBe` ppr p'
            ) inputs