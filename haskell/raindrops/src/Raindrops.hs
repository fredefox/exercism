{-# language ExplicitForAll #-}
{-# language ScopedTypeVariables #-}
{-# language ViewPatterns #-}
module Raindrops (convert, primes) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as List
import Data.Maybe

convert :: forall n . Integral n => Show n => n -> String
convert n
  | null $ [3, 5, 7] `List.intersect` ps = show n
  | otherwise = foldMap go ps
  where
  go :: n -> String
  go = fromMaybe "" . (`lookup` m)
  m :: [(n, String)]
  m = [(3, "Pling"), (5, "Plang"), (7, "Plong")]
  ps = distinctFactorsWhile (<= 7) n

distinctFactorsWhile :: Integral a => Show a => (a -> Bool) -> a -> [a]
distinctFactorsWhile q = distinct . factorsWhile q

factorsWhile :: forall n . Integral n => Show n => (n -> Bool) -> n -> [n]
factorsWhile q = go primes
  where
  go :: [n] -> n -> [n]
  go ps@(p:pss) n
    | n <= 1 = []
    | not (q p) = []
    | n `mod` p == 0 = p : go ps (n `div` p)
    | otherwise = go pss n
  go _ _ = error "IMPOSSIBLE"

distinct :: Eq a => [a] -> [a]
distinct = fmap NonEmpty.head . NonEmpty.group

primes :: Integral n => [n]
primes = 2 : sieve primes [3..]
  where
    sieve (p:ps) xs =
      let (h,t) = span (< p*p) xs
      in  h ++ sieve ps (filter ((/=0).(`mod`p)) t)
    sieve _ _ = error "IMPOSSIBLE"
