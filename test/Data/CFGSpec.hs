
module Data.CFGSpec where

import Test.Hspec
import Data.Cmm.Parser
import TestPrograms
import Data.Either (isRight)
import Data.CFG
import Control.DeepSeq (force)
import Control.Exception (evaluate)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "progToCfg" $ do
    mapM_ testProgToCfg testPrograms
  describe "cfgToProgram" $ do
    mapM_ testCfgToProgram testPrograms

testCfgToProgram :: (String, String) -> SpecWith ()
testCfgToProgram (nm, progstr) =
  it ("should satisfy cfgToProgram . progToCfg = id for " ++ nm) $ do
    let ep = parse program ("program" ++ nm) progstr
    ep `shouldSatisfy` isRight
    let Right p = ep
    p `shouldBe` (cfgToProgram . progToCfg $ p)


testProgToCfg :: (String, String) -> SpecWith ()
testProgToCfg (nm, progstr) =
  it ("should parse convert a program correctly for " ++ nm) $ do
    let ep = parse program ("program" ++ nm) progstr
    ep `shouldSatisfy` isRight
    let Right p = ep
    evaluate . force $ progToCfg p
    return ()