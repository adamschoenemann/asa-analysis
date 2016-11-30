
module Anal.CFGSpec where

import Test.Hspec
import Data.Cmm.Parser
import TestPrograms
import Data.Either (isRight)
import Anal.CFG

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Anal.CFGSpec" $ do
    it "should work" $ do
      mapM_ testCfgProgram testPrograms
      True `shouldBe` True

testCfgProgram :: (String, String) -> IO ()
testCfgProgram (nm, progstr) = do
  let ep = parse program ("program" ++ nm) progstr
  ep `shouldSatisfy` isRight
  let Right p = ep
  let graphName = "cfg_" ++ nm
  putStrLn $ graphName ++ ":"
  putStrLn progstr
  putStrLn ""
  writeVizCfg (cfg p) graphName