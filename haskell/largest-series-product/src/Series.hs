module Series (Error(..), largestProduct) where

import qualified Data.List as List
import Data.Char

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct n xs = case filter (not . isDigit) xs of
  (c:_)               -> Left $ InvalidDigit c
  _ | m <= 0 || n < 0 -> Left InvalidSpan
  _                   -> go xs
  where
  go = pure . solve m n . fmap (fromIntegral . digitToInt)
  m = succ $ length xs - n

solve :: Num n => Ord n => Int -> Int -> [n] -> n
solve m n = maximum . take m . fmap (product . take n) . List.tails
