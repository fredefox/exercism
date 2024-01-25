{-# LANGUAGE LambdaCase, TypeApplications, MultiWayIf #-}
module Luhn (isValid) where

import qualified Data.Char

isValid :: String -> Bool
isValid xs  = case parse xs of
  [_] -> False
  ns   -> (== 0) . (`mod` 10) . sum . convert . reverse $ ns

convert :: [] Int -> [] Int
convert = zipWith ($) (cycle [id, f])
  where
  f n = if
    | m > 9     -> m - 9
    | otherwise -> m
    where
    m = 2 * n

parse :: String -> [] Int
parse = fmap (read @Int . pure) . filter Data.Char.isDigit
