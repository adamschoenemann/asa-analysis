
module Data.Cmm.ParserSpec (main, spec) where

import Data.Cmm.AST()
import Data.Cmm.Parser
import Test.Hspec
import Text.Pretty
import Data.Either (isRight)
import TestUtils

import TestPrograms (testPrograms)

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Data.Cmm.Parser" $ do
    -- of course, this is not generally true, but for these
    -- specific programs, it should be
    mapM_ testParser testPrograms

testParser :: (String, String) -> SpecWith ()
testParser (n, p) =
  it ("satisifed id = ppr. parse for " ++  n) $ do
    let ep = parse program ("program" ++ n) p
    ep `shouldSatisfy` isRight
    let Right p' = ep
    let pprd = ppr p'
    output $ "-----------------" ++ n ++ "------------------"
    output "input: "
    output p
    output "\nexpected:"
    output pprd
    pprd `shouldBe` p