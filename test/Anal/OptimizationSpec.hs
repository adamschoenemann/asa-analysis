
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

opts :: [(String, Optimization)]
opts = [("Dead Code Elim", deadCodeOpt), ("Const Prop", constPropOpt)]

progs :: [(String,[Stmt])]
progs = map (\(n,p) -> (n, either (error "parse error") id $ parse program n p)) testPrograms

spec :: Spec
spec = do
  describe "runOpts" $ do
    it "should work with first constProp and then deadCodeElim on in3" $ do
      let ep = parse program ("program in3") in3
      ep `shouldSatisfy` isRight
      let Right p = ep
      let optProg = runOpts deadCodeOpt constPropOpt p
      let expectProg = [Ass "x" (BLit True),Ass "y" (BLit False),Ass "z" (ILit 1764)]
      optProg `shouldBe` expectProg

    it "should work with first deadCodeElim and then constProp on in3" $ do
      let ep = parse program ("program in3") in3
      ep `shouldSatisfy` isRight
      let Right p = ep
      -- dead-code will only catch constant literals and remove one branch
      -- and const-prop will leave use with more constants, so an extra run
      -- of dead-code would eliminate the last branch
      let optimized = runOpts constPropOpt deadCodeOpt p
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
  describe "optimizeProg (constProp then deadCode)" $ do
    it "should work on in12" $ do
      let ep = parse program ("program in12") in12
      ep `shouldSatisfy` isRight
      let Right p = ep
      let optProg = optimizeProg [deadCodeOpt, constPropOpt] p
      -- putPrettyLn optProg
      let expectProg = [Ass "a" (ILit 59),Ass "b" (ILit 68),Ass "c" (ILit 0),Output (ILit 59)]
      optProg `shouldBe` expectProg

  describe "seqOpts" $ do
    describe "satifises seqOpts [a,b] == runOpts a b" $ do
      let cases = [it ("works for " ++ n ++ " (" ++ an ++ ", " ++ bn ++ ")") $
                        seqOpts [a,b] prog `shouldBe` runOpts a b prog
                  | (an, a) <- opts, (bn, b) <- opts, (n,prog) <- progs
                  ]
      sequence cases
      return ()
--   describe "optimizeCfg" $ do
--     describe "satisifes f opts = f (opts in any permutation)" $ do
--       sequence cases2
--       return ()

-- cases2 :: [SpecWith ()]
-- cases2 =
--       [it ("works for " ++ n ++ " " ++ show (map fst opts')) $ do
--         (optimizeCfg (map snd opts) cfg `shouldBe` optimizeCfg (map snd opts') cfg)
--       | (n,cfg) <- progs, opts' <- permutations opts
--       ]