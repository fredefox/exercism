module RotationalCipher (rotate) where

import Data.Char

rotate :: Int -> String -> String
rotate n = fmap (rot' n)

rot' :: Int -> Char -> Char
rot' n c
  | not (isAlpha c) = c
  | isUpper c = toUpper $ f $ toLower c
  | otherwise = f c
  where
  f = rot toEnum' fromEnum' n

rot :: (Int -> e) -> (e -> Int) -> Int -> e -> e
rot toE frE n x = toE $ frE x + n

fromEnum' :: Char -> Int
fromEnum' c = (fromEnum (toLower c) - fromEnum 'a') `mod` 26

toEnum' :: Int -> Char
toEnum' n = toEnum $ n `mod` 26 + fromEnum 'a'
