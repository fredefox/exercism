module Grains (square, total) where

square :: Integer -> Maybe Integer
square n = safeLookup (pred n) vals

safeLookup :: Integer -> [a] -> Maybe a
safeLookup _ [] = Nothing
safeLookup 0 (x:_) = pure x
safeLookup n (_:xs) = safeLookup (pred n) xs

dbl :: Integer -> Integer
dbl x = x + x

vals :: [Integer]
vals = take 64 $ iterate dbl 1

total :: Integer
total = sum vals
