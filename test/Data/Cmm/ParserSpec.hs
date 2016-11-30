
module Data.Cmm.ParserSpec (main, spec) where

import Data.Cmm.AST()
import Data.Cmm.Parser
import Test.Hspec
import Text.Pretty
import Data.Either (isRight)

import TestPrograms (testPrograms)

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Data.Cmm.Parser" $ do
    -- of course, this is not generally true, but for these
    -- specific programs, it should be
    it "satisfies id = ppr . parse" $ do
      mapM_ (\(n,p) -> do
                let ep = parse program ("program" ++ n) p
                ep `shouldSatisfy` isRight
                let Right p' = ep
                let pprd = ppr p'
                putStrLn "input: "
                putStrLn p
                putStrLn "\nexpected:"
                putStrLn pprd
                p `shouldBe` pprd
            ) testPrograms
