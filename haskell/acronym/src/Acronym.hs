module Acronym (abbreviate) where

import qualified Data.Char

abbreviate :: String -> String
abbreviate = fmap Data.Char.toUpper . (>>= t) . words . fmap go
  where
  go c
    | c == '\''                 = c
    | not (Data.Char.isAlpha c) = ' '
    | otherwise                 = c
  t :: String -> String
  t (x:xs) = x : if all Data.Char.isAsciiUpper xs then [] else filter Data.Char.isAsciiUpper xs
