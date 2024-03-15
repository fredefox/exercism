module Series (slices) where

import Data.List
import Data.Char

slices :: Int -> String -> [[Int]]
slices n xs = take k $ fmap (fmap digitToInt . take n) $ tails xs
  where
  k = succ $ length xs - n
