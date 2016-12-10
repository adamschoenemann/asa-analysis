
module AnnotatedSpec where

import Test.Hspec
import Data.Cmm.Parser
import TestPrograms
import Data.CFG
import Anal
import Text.Pretty
import Data.Cmm.AST
import qualified Data.Map.Lazy as LM
import Data.Cmm.Annotated

main :: IO ()
main = hspec spec

cfgs :: [(String,(CFG,[Stmt]))]
cfgs = map (\(n,p) -> (n, either (error "parse error") (\prg -> (progToCfg prg, prg)) $ parse program n p)) testPrograms

spec :: Spec
spec = do
  describe "cfgToAnnotated" $ do
    describe "id = annotatedToProg . cfgToAnnotated env . progToCfg" $ do
      mapM_ annotateCfg cfgs
      return ()


annotateCfg (n, (cfg, prog)) =
  it ("works for " ++ n) $ do
    let env = LM.fromList $ zip [0..] (repeat UnitLat)
    let ann    = cfgToAnnotated env cfg
    let prog'  = annotatedToProg ann
    putStrLn $ "------------- " ++ n ++ " --------------"
    putStrLn "expected:"
    putPrettyLn prog
    putStrLn "but got:"
    putPrettyLn prog'
    prog' `shouldBe` prog