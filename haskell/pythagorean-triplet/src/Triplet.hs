module Triplet (tripletsWithSum) where

import Control.Monad

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum s = filter (\(a, b, c) -> sum [a,b,c] == s) $ trpl s

trpl :: Int -> [(Int, Int, Int)]
trpl s = do
  a <- [isqrt s..s`div`2]
  (b, c) <- takeWhile (\(b, c) -> sum [a,b,c] <= s) $ do
    let a0 = (s - a * a) `max` 1
    b <- [a0..a]
    let (c, err) = isqrtRem $ p2 a + p2 b
    guard $ err == 0
    pure (b, c)
  pure (b, a, c)
  where
  p2 = (^ (2 :: Int))

isqrt :: Integral n => n -> n
isqrt n
  | n <= 1    = n
  | otherwise = go n0 $ step n0
  where
  n0 = n `div` 2
  go x0 x1
    | x1 < x0 = go x1 (step x1)
    | otherwise = x0
  step x = (x + n `div` x) `div` 2

isqrtRem :: Integral n => n -> (n, n)
isqrtRem n = (r, n - r * r)
  where
  r = isqrt n
