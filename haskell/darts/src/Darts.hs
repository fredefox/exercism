module Darts (score) where

score :: Float -> Float -> Int
score x y
  | d 1       = 10
  | d 5       = 5
  | d 10      = 1
  | otherwise = 0
  where
  d t = dist x y <= t

dist :: Float -> Float -> Float
dist x y = sqrt $ x ^^ two + y ^^ two
  where
  two :: Int
  two = 2
