{-# options_ghc -Wno-x-partial #-}
{-# language ViewPatterns #-}
module RailFenceCipher (encode, decode) where

import Control.Monad
import Data.List

-- encode :: Int -> String -> String
-- encode (pred -> n) s = filter (/= '.') $ join $ (\i -> row i <$> [0..b]) <$> [0..a]
--   where
--   w = arr n s
--   (_, (a, b)) = Array.bounds w
--   row i j = w Array.! (i, j)

-- arr :: Int -> String -> Array (Int, Int) Char
-- arr n s = Array.runSTArray act
--   where
--   act :: forall s . ST s (Array.STArray s (Int, Int) Char)
--   act = do
--     a <- Array.newArray @(Array.STArray s) ((0, 0), (n, pred $ length s)) '.'
--     for_ (zip s (indices n)) $ \(c, ix) -> Array.writeArray a ix c
--     pure a

-- decode :: Int -> String -> String
-- decode (pred -> n) xs = fmap (a Array.!) (init $ takeWhile (Array.inRange (Array.bounds a)) $ indices n)
-- -- decode n xs = a
--   where
--   m = length $ head $ lines xs
--   a = Array.listArray ((0, 0), (n, m)) xs

encode :: Int -> String -> String
encode n xs
  = fmap fst
  $ sortOn snd
  $ zip xs
  $ indices n

decode :: Int -> String -> String
decode n xs
  = fmap fst
  $ sortOn (snd . snd)
  $ zip xs
  $ sort
  $ take (length xs)
  $ indices n

indices :: Int -> [(Int, Int)]
indices (pred -> n) = join $ iterate step ys
  where
  m = 2 * n
  xs = [0..n]
  ys = zip (xs <> init (tail (reverse xs))) [0..]
  step = fmap (\(a, b) -> (a, b + m))
