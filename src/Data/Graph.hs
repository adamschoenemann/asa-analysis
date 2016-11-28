
{-# LANGUAGE NamedFieldPuns #-}

module Data.Graph where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Set (Set)

data Node       a = Node ID a deriving (Show)
data Edge       e = Edge { endpoints :: (ID,ID), info :: e } deriving (Show, Ord, Eq)

type ID = Int
type Nodes      a = Map ID (Node a)


data Graph a e = Graph
  { nodes  :: Nodes a --Map ID (Node a)
  , edges  :: Set (Edge e)
  , src    :: ID
  , sink   :: ID
  } deriving (Show)

data Graphvizer a e =
  Graphvizer { vizNode :: Graph a e -> Node a -> String
             , vizEdge :: Graph a e -> Edge e -> String
             }

toGraphviz :: Graph a e -> String -> Graphvizer a e -> String
toGraphviz g@(Graph {nodes, edges}) name gvzer =
  let nodesStr = unlines $ map (("\t" ++) . (vizNode gvzer $ g) . snd) $ M.toList nodes
      edgesStr = unlines $ map (("\t" ++) . (vizEdge gvzer) g) $ S.toList edges
  in "digraph " ++ name ++ " {\n" ++
        nodesStr ++ "\n" ++ edgesStr ++
        "}"
