module Matrix (saddlePoints) where

import Data.Array (Array, Ix, (!))
import qualified Data.Array as Array
import qualified Data.List as List
import Data.Function

saddlePoints :: Ix i => Array (i, i) e -> [(i, i)]
saddlePoints matrix = error "You need to implement this function."

solve a = fmap fst (rowsMax a) `pr` fmap fst (colsMin a)
  where
  pr = (,)

a = Array.listArray ((0, 0), (2, 3)) [9, 8, 7, 8, 5, 3, 2, 4, 6, 6, 7, 1]

col :: Ix i => i -> Array (i, i) e -> [((i, i), e)]
col i a = p <$> Array.range ((n, i), (m, i))
  where
  ((n,_), (m,_)) = Array.bounds a
  p ix = (ix, a ! ix)

colMin i = minimumOn snd . col i

colsMin a = (`colMin` a) <$> (enumFromTo n (pred m))
  where
  ((n,_), (m,_)) = Array.bounds a

rowMax i = maximumOn snd . row i

rowsMax a = (`rowMax` a) <$> (enumFromTo n (pred m))
  where
  ((_,n), (_,m)) = Array.bounds a

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn p = List.maximumBy (compare `on` p)

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn p = List.minimumBy (compare `on` p)

row :: Ix i => i -> Array (i, i) e -> [((i, i), e)]
row i a = p <$> Array.range ((i, n), (i, m))
  where
  ((_,n), (_,m)) = Array.bounds a
  p ix = (ix, a ! ix)
