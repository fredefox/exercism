module School (add, empty, grade, sorted) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe

add :: Ord k => Ord v => k -> v -> Map k (Set v) -> Map k (Set v)
add k v = Map.alter (pure . Set.insert v . fromMaybe mempty) k

empty :: Map Int (Set String)
empty = mempty

grade :: Ord k => Ord v => k -> Map k (Set v) -> [v]
grade k = Set.toList . fromMaybe mempty . Map.lookup k

sorted :: Map k (Set v) -> [(k, [v])]
sorted = Map.toList . fmap Set.toList
