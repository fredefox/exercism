module Isogram (isIsogram) where

import Data.List
import Data.Char

isIsogram :: String -> Bool
isIsogram
  = all (lengthAtMost 1)
  . group
  . sort
  . fmap toLower
  . filter isLetter

lengthAtMost :: Int -> [a] -> Bool
lengthAtMost n _ | n < 0 = False
lengthAtMost _ [] = True
lengthAtMost n (_:xs) = lengthAtMost (pred n) xs
