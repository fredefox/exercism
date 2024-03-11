module Affine (decode, encode) where

import Data.Char
import Control.Applicative

decode :: (Int, Int) -> String -> Maybe String
decode (a, b) xs
  | gcd a m /= 1 = Nothing
  | otherwise = pure $ foldMap f' xs
  where
  f' c
    | isAlpha c = pure $ f c
    | isDigit c = pure c
    | otherwise = empty
  f :: Char -> Char
  f c = toEnum' $ (mmi m a * (fromEnum' c - b)) `mod` m
  m = 26

mmi :: Int -> Int -> Int
mmi m a = let (_, x, _) = extGCD m a in x

encode :: (Int, Int) -> String -> Maybe String
encode (a, b) xs
  | gcd a m /= 1 = Nothing
  | otherwise
    = pure
    $ unwords
    $ chunksOf 5
    $ foldMap f' xs
  where
  f' c
    | isAlpha c = pure $ f $ toLower c
    | isDigit c = pure c
    | otherwise = empty
  f c = toEnum' $ (a * fromEnum' c + b) `mod` m
  m = 26

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = a : chunksOf n b
  where
  (a, b) = splitAt n xs

toEnum' :: Int -> Char
toEnum' n = toEnum (n + fromEnum 'a')

fromEnum' :: Char -> Int
fromEnum' c = fromEnum c - fromEnum 'a'

extGCD :: Integral n => n -> n -> (n,n,n)
extGCD 0 0 = error "Affince.extGCD: `extGCD 0 0` is not defined"
extGCD a 0 = (1,0,a)
extGCD a b = (x,c-q*x,y)
  where
  (q,r) = a `divMod` b
  (c,x,y) = extGCD b r
