module CollatzConjecture (collatz) where

go :: Integer -> Integer -> Maybe Integer
go n acc
  | n <= 0    = Nothing
  | n == 1    = pure acc
  | otherwise = go (next n) (succ acc)

next :: Integer -> Integer
next n
  | even n    = n `div` 2
  | otherwise = succ $ 3 * n

collatz :: Integer -> Maybe Integer
collatz n = go n 0
