{-# LANGUAGE LambdaCase, BangPatterns #-}
module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f !z = \case
  [] -> z
  (a:as) -> foldl' f (f z a) as

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z = \case
  [] -> z
  (a:as) -> f a (foldr f z as)

length :: [a] -> Int
length = foldr (const succ) 0

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\a acc -> f a : acc) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\a acc -> if p a then a : acc else acc) []

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat = foldr (++) []
