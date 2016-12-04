
module GraphVizGen where

import TestPrograms
import Data.CFG (progToCfg, writeVizCfg)
import Data.Cmm.Parser (parse, program)

generateGviz :: IO ()
generateGviz = mapM_ fun testPrograms where
  fun (nm, prog) = either (error "did not parse") (\p -> writeVizCfg (progToCfg p) nm) $
                      parse program nm prog
