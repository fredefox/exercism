module Main (main) where

import System.Environment
import Data.Foldable

go :: String -> Int -> Int -> Maybe String
go s n m = if m `mod` n == 0 then Just s else Nothing

solve :: Int -> String
solve n = case mconcat [go "Pling" 3, go "Plang" 5, go "Plong" 7] n of
  Nothing -> show n
  Just s -> s

main :: IO ()
main = getArgs >>= traverse_ (putStrLn . solve . read)
