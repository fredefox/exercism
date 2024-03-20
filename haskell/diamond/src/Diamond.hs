module Diamond (diamond) where

import Data.Char

import Prelude hiding (lines)

diamond :: Char -> Maybe [String]
diamond c
  | isAsciiUpper c = pure $ solve c
  | otherwise = Nothing

solve :: Char -> [String]
solve c = lines n ' ' k
  where
  n = fromEnum c - fromEnum 'A'
  k i = toEnum (i + fromEnum 'A')

lines :: Int -> a -> (Int -> a) -> [[a]]
lines n a b = (\x -> line n x a (b x)) <$> ys
  where
  xs = [0..n]
  ys = xs <> tail (reverse xs)

line :: Int -> Int -> a -> a -> [a]
line n m a b
  | m == 0 = replicate n' a <> pure b <> replicate n' a
  | otherwise =  replicate n' a <> pure b <> replicate m' a <> pure b <> replicate n' a
  where
  n' = n - m
  m' = succ $ 2 * (pred m)
