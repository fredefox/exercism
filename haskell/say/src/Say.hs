{-# options_ghc -Wall #-}
module Say (inEnglish) where

import qualified Data.List as List
import Data.Maybe
import Control.Applicative

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 = empty
  | otherwise = pure $ solve n

digit :: Integral n => n -> String
digit n = snd $ lkp n

solve :: Integral n => Show n => n -> String
solve n
  | n < 0    = "negative " <> solve (negate n)
  | n <= 20  = digit n
  | n <  100 = let (a, b) = n `divMod` 10 in tens a <> "-" <> digit b
  | otherwise = case lkp n of
    (m, s) -> d <> " " <> s <> b'
      where
      (a, b) = n `divMod` m
      d = if a == 1 && m < 100 then "" else solve a
      b' = if b == 0 then [] else " " <> solve b

tens :: Integral n => n -> String
tens n = snd $ lkp (10 * n)

lkp :: Integral n => n -> (n, String)
lkp n = fromMaybe undefined $ List.find (\(a,_) -> n >= a) table

table :: Integral n => [(n, String)]
table = reverse
  [ 0 % "zero"
  , 1 % "one"
  , 2 % "two"
  , 3 % "three"
  , 4 % "four"
  , 5 % "five"
  , 6 % "six"
  , 7 % "seven"
  , 8 % "eight"
  , 9 % "nine"
  , 10 % "ten"
  , 11 % "eleven"
  , 12 % "twelve"
  , 13 % "thirteen"
  , 14 % "fourteen"
  , 15 % "fifteen"
  , 16 % "sixteen"
  , 17 % "seventeen"
  , 18 % "eighteen"
  , 19 % "nineteen"
  , 20 % "twenty"
  , 30 % "thirty"
  , 40 % "forty"
  , 50 % "fifty"
  , 60 % "sixty"
  , 70 % "seventy"
  , 80 % "eighty"
  , 90 % "ninety"
  , 100 % "hundred"
  , 1000 % "thousand"
  , 1000000 % "million"
  , 1000000000 % "billion"
  , 1000000000000 % "trillion"
  , 1000000000000000 % "quadrillion"
  ]
  where
  (%) = (,)
