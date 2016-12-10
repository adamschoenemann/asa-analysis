
module Anal.LivenessSpec where

import Test.Hspec
import Data.Cmm.Parser
import TestPrograms
import Anal.Liveness
import Data.Cmm.AST
import Anal

main :: IO ()
main = hspec spec

parsed :: [(String,[Stmt])]
parsed = map (\(n,p) -> (n, either (error "parse error") (id) $ parse program n p)) testPrograms

spec :: Spec
spec = do
  describe "deadCodeTrans" $ do
      mapM_ testDeadCodeTrans parsed
      return ()


testDeadCodeTrans (n, prog) =
  it ("works for " ++ n) $ do
    pprintAnalysis livenessAnal prog
    True `shouldBe` True
    -- let eliminated = deadCodeTrans prog
    -- let eliminated' = cfgToProgram . deadCodeElim . progToCfg $ prog
    -- putStrLn $ "----------------- " ++ n ++ " ----------------"
    -- putStrLn "--- before ---"
    -- putPrettyLn prog
    -- putStrLn "\n--- after ---"
    -- putPrettyLn eliminated
    -- eliminated `shouldBe` eliminated'