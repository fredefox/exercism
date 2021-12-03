{-# LANGUAGE TypeApplications, MultiWayIf #-}
module Luhn (main, isValid) where

import qualified System.Exit
import qualified Data.Char
import Data.Foldable

main :: IO ()
main = do
  xs <- lines <$> getContents
  let ys = filter (not . isValid) xs
  traverse_ putStrLn ys
  case ys of
    [] -> System.Exit.exitSuccess
    _  -> System.Exit.exitFailure

isValid :: String -> Bool
isValid [_] = False
isValid xs  = (== 0) . (`mod` 10) . sum . convert . parse $ xs

convert :: [] Int -> [] Int
convert = zipWith ($) (cycle [f, id])
  where
  f n = if
    | m > 9     -> m - 9
    | otherwise -> m
    where
    m = 2 * n

parse :: String -> [] Int
parse = fmap (read @Int . pure) . filter Data.Char.isDigit
