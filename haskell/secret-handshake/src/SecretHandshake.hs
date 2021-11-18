{-# language TypeApplications #-}
module SecretHandshake (handshake) where

import Data.Foldable
import Data.Bits
import Control.Monad

handshake :: Int -> [String]
handshake n = (if "" `elem` ys then reverse else id) $ filter (/= "") $ ys
  where
  go :: Int -> String -> [String]
  go i s
    | testBit n i = pure s
    | otherwise = mempty
  ys = join $ zipWith go [0..4] ["wink", "double blink", "close your eyes", "jump", ""]

main :: IO ()
main = getContents >>= traverse_ (print . handshake . read @Int) . lines
