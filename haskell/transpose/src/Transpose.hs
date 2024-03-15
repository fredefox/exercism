module Transpose (transpose) where

import qualified Data.List

transpose :: [String] -> [String]
transpose xs = Data.List.transpose $ pads ' ' xs

pads :: a -> [[a]] -> [[a]]
pads p xs = xs'
  where
  ns = reverse $ tail $ scanl (\acc x -> acc `max` length x) 0 $ reverse xs
  xs' = zipWith (rpad p) ns xs

rpad :: a -> Int -> [a] -> [a]
rpad _ 0 xs = xs
rpad x0 n [] = replicate n x0
rpad x0 n (x:xs) = x : rpad x0 (pred n) xs
