module Minesweeper (main, annotate) where

import Data.List

annotate :: [String] -> [String]
annotate xs = fmap (fmap (either id (head . show))) $ zipWith f lr ud
  where
  lr = fmap onedir xs
  ud = transpose . fmap onedir . transpose $ xs
  f :: [Either Char Int] -> [Either Char Int] -> [Either Char Int]
  f = zipWith g
  g :: Either Char Int -> Either Char Int -> Either Char Int
  g (Left '*') _ = Left '*'
  g Left{} a = a
  g _ (Left '*') = Left '*'
  g a Left{} = a
  g (Right n) (Right m) = Right (n + m)

onedir :: String -> [Either Char Int]
onedir xs = zipWith f l xs
  where
  k = (\c -> if c == '*' then 1 else 0) <$> xs
  l = zipWith (+) (tail k <> [0]) (0 : init k)
  f :: Int -> Char -> Either Char Int
  f n c
    | c == '*'  = Left c
    | n == 0    = Left c
    | otherwise = Right n

main :: IO ()
main = getContents >>= putStrLn . unlines . annotate . lines
