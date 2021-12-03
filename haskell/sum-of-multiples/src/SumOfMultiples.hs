{-# LANGUAGE LambdaCase #-}
module SumOfMultiples (sumOfMultiples) where

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = case compare x y of
  LT -> x : merge xs (y:ys)
  EQ -> x : merge xs ys
  GT -> y : merge (x:xs) ys

mult :: [Integer] -> [Integer]
mult = foldr merge [] . fmap go
  where
  go = \case
    0 -> []
    x -> enumFromThen 0 x

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ takeWhile (< limit) $ mult factors
