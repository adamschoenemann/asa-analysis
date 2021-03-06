
module Data.Cmm.QuasiQuoter where

import Data.Cmm.Parser
import Data.Cmm.AST
import Language.Haskell.TH.Quote

cmm :: QuasiQuoter
cmm = QuasiQuoter
  { quoteExp = quoteCmmExp
  , quotePat = undefined -- quoteCmmPat
  , quoteDec = undefined
  , quoteType = undefined
  }

quoteParseCmm :: Monad m => String -> m Program
quoteParseCmm s = either (fail . show) return $ parseCmm s

quoteCmmExp s = do
  -- pos <- getPosition
  prog <- quoteParseCmm s
  dataToExpQ (const Nothing) prog

-- antiCmmPat :: Program -> Maybe (TH.Q TH.Pat)
-- antiCmmPat



