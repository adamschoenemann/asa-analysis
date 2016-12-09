
module Anal.ConstPropSpec where

import Test.Hspec
import Data.Cmm.Parser
import TestPrograms
import Data.Either (isRight)
import Anal.ConstProp
import Anal.DeadCode
import Anal
import Data.Cmm.AST
import Data.CFG
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Utils (unsafeLookup)
import TestUtils
import Control.DeepSeq (force)
import Control.Exception (evaluate)

-- expected :: Map String [(Int, [Expr])]
-- expected = M.fromList [("in1", exp1), ("in2", exp2), ("in3", exp3), ("in4", exp4), ("in5", exp5)]

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

-- testAnalysis (nm, progstr) = do
--   it ("should work for " ++ nm) $ do
--     let ep = parse program ("program" ++ nm) progstr
--     ep `shouldSatisfy` isRight
--     let Right p = ep
--     let result = analyzeProg constProp p
--     -- putStrLn $ nm ++ ":"
--     -- pprintAnalysis constProp p
--     return ()
--     -- (map tupSetToList $ M.toList result) `shouldBe` unsafeLookup nm expected

tupSetToList :: (a, Set b) -> (a, [b])
tupSetToList (k,s) = (k, S.toList s)