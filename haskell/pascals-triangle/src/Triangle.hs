module Triangle (rows) where

import Data.Foldable
import System.Environment

rows :: Int -> [[Integer]]
rows n = take n $ solve

solve :: [[Integer]]
solve = iterate step [1]

step :: Num n => [n] -> [n]
step xs = zipWith (+) (0 : xs) (xs <> [0])

main :: IO ()
main = traverse_ print solve
