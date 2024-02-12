{-# language ExplicitForAll #-}
{-# language ScopedTypeVariables #-}
module Raindrops (convert) where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as List
import Data.Maybe

convert :: Int -> String
convert n
  | null $ [3, 5, 7] `List.intersect` ns = show n
  | otherwise = foldMap go ns
  where
  go :: Int -> String
  go = fromMaybe mempty . (`lookup` m)
  m :: [(Int, String)]
  m = [(3, "Pling"), (5, "Plang"), (7, "Plong")]
  ns = distinctFactors n

distinctFactors :: Integral a => a -> [a]
distinctFactors = distinct . factors

factors :: forall n . Integral n => n -> [n]
factors = go primes
  where
  go :: [n] -> n -> [n]
  go ps@(p:pss) n
    | n <= 1 = []
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
