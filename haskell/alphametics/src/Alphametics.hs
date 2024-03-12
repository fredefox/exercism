{-# language TypeApplications #-}
{-# language PartialTypeSignatures #-}
module Alphametics (solve) where

import Data.Char
import Control.Monad
import Data.List
import Data.Maybe

solve :: String -> Maybe [(Char, Int)]
solve = parse >=> solve2

-- Quick and dirty implementation
parse :: String -> Maybe ([String], String)
parse = pure . f . words . filter ((||) <$> isAlpha <*> isSpace)
  where
  f xs = (init xs, last xs)

solve2 :: ([String], String) -> Maybe [(Char, Int)]
solve2 gamma@(xs, x) = find (isValid gamma) $ assgn alpha
  where
  alpha = nub $ join $ x : xs

isValid :: ([String], String) -> [(Char, Int)] -> Bool
isValid (xs, x) env = sum (fmap (replace env) xs) == replace env x

replace :: _ -> String -> Int
replace env xs = read $ fmap f xs
  where
  f c = intToDigit $ fromMaybe undefined $ lookup c env

assgn :: [a] -> [[(a, Int)]]
assgn [] = [[]]
assgn (x:xs) = do
  i <- [1..9]
  fmap ((x, i) :) (assgn xs)
