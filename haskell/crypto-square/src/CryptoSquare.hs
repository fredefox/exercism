{-# LANGUAGE TypeApplications #-}
module CryptoSquare (main, encode) where

import qualified Data.Char

main :: IO ()
main = do
  xs <- getContents
  putStrLn $ encode xs

normalize :: String -> String
normalize xs = Data.Char.toLower <$> filter Data.Char.isAlphaNum xs

encode :: String -> String
encode xs' = unwords $ transpose $ rpad c <$> chunksOf c xs
  where
  c = ceiling $ sqrt $ fromIntegral @_ @Double $ length xs
  xs = normalize xs'

chunksOf :: Int -> [] a -> [[a]]
chunksOf _ [] = []
chunksOf n xs = a : chunksOf n b
  where
  (a, b) = splitAt n xs

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose [x] = pure <$> x
transpose (x:xs) = zipWith (:) x (transpose xs)

rpad :: Int -> String -> String
rpad n xs = xs <> replicate (n - m) ' '
  where
  m = length xs
