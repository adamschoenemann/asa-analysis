
module Anal.ConstPropSpec where

import Test.Hspec
import Data.Cmm.Parser
import TestPrograms
import Data.Either (isRight)
import Anal.ConstProp
import Anal.DeadCode
import Anal
import Data.Set (Set)
import qualified Data.Set as S
import Control.DeepSeq (force)
import Control.Exception (evaluate)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- describe "analysis" $ do
  --   mapM_ testAnalysis $ testPrograms
  describe "optimization" $ do
    mapM_ testOptimization $ testPrograms

testOptimization (nm, progstr) =
  it nm $ do
    let ep = parse program ("program" ++ nm) progstr
    ep `shouldSatisfy` isRight
    let Right p = ep
    (evaluate . force) $ optimizeProg [constPropOpt, deadCodeOpt] p
    True `shouldBe` True
    return ()

testAnalysis (nm, progstr) = do
  it ("should work for " ++ nm) $ do
    let ep = parse program ("program" ++ nm) progstr
    ep `shouldSatisfy` isRight
    let Right p = ep
    let result = analyzeProg constProp p
    (evaluate . force) result
    -- putStrLn $ nm ++ ":"
    -- pprintAnalysis constProp p
    return ()

tupSetToList :: (a, Set b) -> (a, [b])
tupSetToList (k,s) = (k, S.toList s)