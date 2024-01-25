{-# options_ghc -Wall -Werror #-}
module Minesweeper (main, annotate) where

import Data.Array (Array)
import qualified Data.Array as Array
import Control.Monad
import qualified Data.Char as Char

matrix :: [[a]] -> Array (Int, Int) a
matrix xs = Array.listArray ((0, 0), (n, m)) $ join xs
  where
  n = pred $ length xs
  m = pred $ length $ case xs of { (x:_) -> x ; _ -> [] }

array :: Array.Ix i => (i, i) -> (i -> e) -> Array i e
array ix f = Array.array ix $ (\i -> (i, f i)) <$> Array.range ix

toList :: Array (Int, Int) a -> [[a]]
toList a
  | n == -1 = []
  | m == -1 = [[]]
  | otherwise = chunksOf (succ m) $ Array.elems a
  where
  (_, (n, m)) = Array.bounds a

chunksOf :: Int -> [a] -> [[a]]
chunksOf n _ | n <= 0 = []
chunksOf _ [] = []
chunksOf n xs = a : chunksOf n b
  where
  (a, b) = splitAt n xs

annotate :: [String] -> [String]
annotate xs = toList w
  where
  m = matrix xs
  w = array (Array.bounds m) f
  f :: (Int, Int) -> Char
  f ix = case m Array.! ix of
    '*' -> '*'
    _ -> if k == 0 then ' ' else Char.intToDigit k
    where
    k = length $ filter (== '*') $ neighbors m ix

neighbors :: Array (Int, Int) a -> (Int, Int) -> [a]
neighbors m (i, j) = (m Array.!) <$> filter (Array.inRange b) ixs
  where
  b = Array.bounds m
  ixs = 
    [ (pred i, pred j)
    , (pred i, j)
    , (pred i, succ j)
    , (     i, pred j)
    , (     i, succ j)
    , (succ i, pred j)
    , (succ i,      j)
    , (succ i, succ j)
    ]

main :: IO ()
main = getContents >>= putStrLn . unlines . annotate . lines
