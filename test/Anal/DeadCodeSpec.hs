
module Anal.DeadCodeSpec where

import Test.Hspec
import Data.Cmm.Parser
import TestPrograms
import Data.CFG
import Data.Either (isRight)
import Anal
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Anal.DeadCode
import TestUtils
import Text.Pretty
import Data.Cmm.AST
import Data.List (permutations)
import qualified Data.Map.Lazy as LM
import Data.Lat
import Annotated

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
    let eliminated = deadCodeTrans prog
    let eliminated' = cfgToProgram . deadCodeElim . progToCfg $ prog
    -- putStrLn $ "----------------- " ++ n ++ " ----------------"
    -- putStrLn "--- before ---"
    -- putPrettyLn prog
    -- putStrLn "\n--- after ---"
    -- putPrettyLn eliminated
    eliminated `shouldBe` eliminated'