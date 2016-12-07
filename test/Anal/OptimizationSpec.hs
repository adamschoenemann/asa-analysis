
module Anal.OptimizationSpec where

import Test.Hspec
import Data.Cmm.Parser
import TestPrograms
import Data.CFG
import Data.Either (isRight)
import Anal
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Anal.DeadCode
import Anal.ConstProp
import TestUtils
import Text.Pretty

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "runOpts" $ do
    it "should work with first constProp and then deadCodeElim on in3" $ do
      let ep = parse program ("program in3") in3
      ep `shouldSatisfy` isRight
      let Right p = ep
      let cfg = progToCfg p
      let optimized = runOpts deadCodeOpt constPropOpt cfg
      putPrettyLn . cfgToProgram $ optimized
      return ()
    it "should work with first deadCodeElim and then constProp on in3" $ do
      let ep = parse program ("program in3") in3
      ep `shouldSatisfy` isRight
      let Right p = ep
      let cfg = progToCfg p
      let optimized = runOpts constPropOpt deadCodeOpt cfg
      putPrettyLn . cfgToProgram $ optimized
      return ()