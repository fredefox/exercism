{-# language ViewPatterns #-}
module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random
import Control.Monad

caesarDecode :: String -> String -> String
caesarDecode (cycle -> xs) = zipWith (shiftWith subtract) xs

caesarEncode :: String -> String -> String
caesarEncode (cycle -> xs) = zipWith (shiftWith (+)) xs

shiftWith :: (Int -> Int -> Int) -> Char -> Char -> Char
shiftWith f (fromEnum' -> k) (fromEnum' -> v)
  = toEnum' $ f k v

fromEnum' :: Char -> Int
fromEnum' c = (fromEnum c - fromEnum 'a') `mod` 26

toEnum' :: Int -> Char
toEnum' n = toEnum (n `mod` 26 + fromEnum 'a')

rand :: Int -> IO String
rand k = do
  n <- randomIO
  ns <- replicateM (n `mod` k) randomIO
  pure $ fmap (toEnum' . (`mod` 26)) ns

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom v = do
  k <- rand 255
  pure (k, caesarEncode k v)
