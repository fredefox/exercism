{-# language LambdaCase, TypeApplications, ScopedTypeVariables #-}
{-# options_ghc -Wno-all #-}
module Dom (chain) where

import Control.Applicative
import Control.Monad
import qualified Data.List as List
import Data.Maybe (listToMaybe)
import Data.Graph (Graph, Vertex, Edge)
import qualified Data.Graph as Graph
import qualified Data.Array as Array
import Data.Tree (Tree(Node), Forest)
import qualified Data.Tree as Tree
import Data.List (nub)

chain :: [Edge] -> Maybe [Edge]
chain es = path p $ fmap (fmap node) $ Graph.dff $ g
  where
  (g, f, _) = line $ build es
  node = (\(e, _, _) -> e) . f
  (_, a) `p` (b, _) = a == b

foo vs = putStr $ Tree.drawForest $ fmap (fmap (show . f')) ts
  where
  (g, f, _) = line $ build vs
  ts = Graph.dff g
  f' = (\(e, _, _) -> e) . f

path :: forall a . (a -> a -> Bool) -> Forest a -> Maybe [a]
path p = go >=> q
  where
  go = \case
    [] -> pure []
    [Node x xs] -> (x :) <$> go xs
    _ -> Nothing
  q :: [a] -> Maybe [a]
  q = \case
    xs@(x:_) -> xs <$ guard (last xs `p` x)
    _ -> Just []

build :: [Edge] -> Graph
build [] = Graph.buildG (0, 0) []
build vs = Graph.buildG (a, b) $ nub $ foldMap (\(t, u) -> [(t, u), (u, t)]) vs
  where
  xs = foldMap (\(t, u) -> [t,u]) vs
  a = minimum xs
  b = maximum xs

-- | `line g` is the "line graph" of the graph `g`.
line :: Graph -> (Graph, Vertex -> (Edge, Edge, [Edge]), Edge -> Maybe Vertex)
line g = Graph.graphFromEdges @Edge @Edge $ edges g
  where
  es :: [Graph.Edge]
  es = Graph.edges g
  b :: Graph.Bounds
  b = Array.bounds g

-- | `edges g` is the adjacency list of the line graph of `g`.
edges :: Graph -> [(Edge, Edge, [Edge])]
edges g = go <$> Graph.edges g
  where
  go :: Edge -> (Edge, Edge, [Edge])
  go v@(t, u) = (v, v, (\u' -> (u, u')) <$> g Array.! u)
