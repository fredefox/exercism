module Squares (difference, squareOfSum, sumOfSquares) where

import Prelude hiding ((^))
import qualified Prelude

(^) :: Num n => n -> Int -> n
x ^ n = x Prelude.^ n

difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n

squareOfSum :: Integral a => a -> a
squareOfSum n = (n ^ 2 * (n + 1) ^ 2) `div` 4

sumOfSquares :: Integral a => a -> a
sumOfSquares n = (n * (n + 1) * (2 * n + 1)) `div` 6
