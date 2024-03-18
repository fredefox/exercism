module Atbash (decode, encode) where

import Data.Char

decode :: String -> String
decode = fmap rotate . filter isAlphaNum

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = a : chunksOf n b
  where
  (a, b) = splitAt n xs

encode :: String -> String
encode = unwords . chunksOf 5 . fmap rotate . filter isAlphaNum

rotate :: Char -> Char
rotate c
  | isLetter c = toEnum $ rot $ fromEnum $ toLower c
  | otherwise  = c

rot :: Int -> Int
rot n = 97 + (122 - n) `mod` 26
