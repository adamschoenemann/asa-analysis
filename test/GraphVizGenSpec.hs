
module GraphVizGenSpec where

import TestPrograms
import Test.Hspec
import Data.CFG (progToCfg, writeVizCfg)
import Data.Cmm.Parser (parse, program)

main :: IO ()
main = hspec spec

spec :: Spec
spec = it "generates graphviz file" $ generateGviz

generateGviz :: IO ()
generateGviz = mapM_ fun testPrograms where
  fun (nm, prog) = either (error "did not parse") (\p -> writeVizCfg (progToCfg p) nm) $
                      parse program nm prog
