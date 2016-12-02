
module GraphVizGen where

import TestPrograms
import Anal.CFG (cfg, writeVizCfg)
import Data.Cmm.Parser (parse, program)

generateGviz :: IO ()
generateGviz = mapM_ fun testPrograms where
  fun (nm, prog) = either (error "did not parse") (\p -> writeVizCfg (cfg p) nm) $
                      parse program nm prog
