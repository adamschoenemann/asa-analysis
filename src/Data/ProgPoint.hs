
module Data.ProgPoint where

import Data.CFG
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M

data ProgPoint =
  PP { dependent :: Set ID, node :: (ID, Node)}
     deriving (Show)

cfgToProgP :: CFG -> [ProgPoint]
cfgToProgP (CFG nodes) = M.elems . M.mapWithKey nodeToPP $ nodes where
  nodeToPP k node = case node of
      Source o              -> PP empty    (k,node)
      Single stmt i o       -> PP (pure i) (k,node)
      CondITE e i bt bf     -> PP (pure i) (k,node)
      CondWhile e i bt bf   -> PP (pure i) (k,node)
      Confluence (i1, i2) o -> PP (S.fromList [i1, i2]) (k,node)
      Sink i                -> PP (pure i) (k,node)
    where pure x = S.singleton x
          empty  = S.empty