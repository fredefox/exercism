module BinarySearch (find) where

import Data.Array
import qualified Data.Array as Array

find :: Ord a => Array Int a -> a -> Maybe Int
find w x = go $ Array.bounds w
  where
  go :: (Int, Int) -> Maybe Int
  go (a, b)
    | a > b = Nothing
    | otherwise = case x `compare` (w Array.! ab) of
        LT -> go (a, pred ab)
        EQ -> pure ab
        GT -> go (succ ab, b)
    where
    ab = a + ((b - a) `div` 2)
