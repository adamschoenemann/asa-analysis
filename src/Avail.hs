{-# LANGUAGE NamedFieldPuns, FlexibleInstances, QuasiQuotes #-}

module Avail where

-- import Data.Cmm.AST
import Anal
import Anal.QuasiQuoter
-- import Utils
-- import Data.Map.Strict (Map)
-- import Data.CFG
-- import qualified Data.Map.Strict as M
-- import Control.Monad.State
-- import Debug.Trace
-- import Data.Set (Set)
-- import qualified Data.Set as S
-- import Data.Functor ((<$>))

cmm1 = [cmm|x := 10;|]

cmm2 = [cmm|
  x := 10;
  z := 0;
  while z < x do {
    output z;
    z := z + 1;
  }
  output z;
  |]

cmm3 = [cmm|
  output 10;
  z := 0;
  if true then {
    output z;
  } else {
    output x;
  }
|]