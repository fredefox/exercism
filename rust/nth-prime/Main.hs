module Main (main) where

import Data.Foldable

go :: [Int] -> [Int]
go (n:ns) = n : go (filter (\m -> m `mod` n /= 0) ns)

primes :: [Int]
primes = go [2..]

main :: IO ()
main = traverse_ print primes
