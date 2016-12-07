
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
import Data.Cmm.AST
import Data.List (permutations)

main :: IO ()
main = hspec spec

opts :: [Optimization]
opts = [deadCodeOpt, constPropOpt]

cfgs :: [CFG]
cfgs = map (\(n,p) -> either (error "parse error") progToCfg $ parse program n p) testPrograms

spec :: Spec
spec = do
  describe "runOpts" $ do
    it "should work with first constProp and then deadCodeElim on in3" $ do
      let ep = parse program ("program in3") in3
      ep `shouldSatisfy` isRight
      let Right p = ep
      let cfg = progToCfg p
      let optCFG = runOpts deadCodeOpt constPropOpt cfg
      let optProg = cfgToProgram $ optCFG
      let expectProg = [Ass "x" (BLit True),Ass "y" (BLit False),Ass "z" (ILit 1764)]
      optProg `shouldBe` expectProg

    it "should work with first deadCodeElim and then constProp on in3" $ do
      let ep = parse program ("program in3") in3
      ep `shouldSatisfy` isRight
      let Right p = ep
      let cfg = progToCfg p
      -- dead-code will only catch constant literals and remove one branch
      -- and const-prop will leave use with more constants, so an extra run
      -- of dead-code would eliminate the last branch
      let optimized = cfgToProgram $ runOpts constPropOpt deadCodeOpt cfg
      let expected =
            [ Ass "x" (BLit True)
            , ITE (BLit True)
                  (Block [ Ass "y" (BLit False)
                         , Ass "z" (ILit 1764)
                         ]
                  )
                  (Ass "y" (ILit 0))
            ]
      optimized `shouldBe` expected
  describe "seqOpts" $ do
    it "satifises seqOpts [a,b] == runOpts a b" $ do
      let cases = [seqOpts [a,b] cfg `shouldBe` runOpts a b cfg
                  | a <- opts, b <- opts, cfg <- cfgs
                  ]
      sequence cases
      return ()
  describe "optimizeCfg" $ do
    it "satisifes f opts = f (opts in any permutation)" $ do
      let cases =
            [optimizeCfg opts cfg `shouldBe` optimizeCfg opts' cfg
            | cfg <- cfgs, opts' <- permutations opts
            ]
      sequence cases
      return ()
