
module Data.ProgPoint where

import Data.CFG (Node, ID)
import Data.Set (Set)

data ProgPoint =
  PP { dependent :: Set ID, node :: (ID, Node)}
     deriving (Show)

