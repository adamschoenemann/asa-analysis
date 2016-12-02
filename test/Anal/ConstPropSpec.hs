
module Anal.ConstPropSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Anal.Available" $ do
    it "should work" $ do
      True `shouldBe` True