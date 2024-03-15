{-# language GHC2021 #-}
module Scrabble (scoreLetter, scoreWord) where

import Data.Maybe
import Control.Applicative
import Data.Char

(|>) :: [a] -> b -> [(a, b)]
xs |> x = (,x) <$> xs

infixl 4 |>

scoreLetter :: Char -> Integer
scoreLetter = fromMaybe 0 . (`lookup` m) . toLower
  where
  m =   "aeioulnrst"  |> 1
    <|> "dg"          |> 2
    <|> "bcmp"        |> 3
    <|> "fhvwy"       |> 4
    <|> "k"           |> 5
    <|> "jx"          |> 8
    <|> "qz"          |> 10

scoreWord :: String -> Integer
scoreWord = sum . fmap scoreLetter
